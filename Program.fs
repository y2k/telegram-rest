﻿module TelegramRest

open System

module Telegram =
    open TLSharp.Core
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    type Msg = 
        | ResetClient of appId : int * apiHash : string
        | Connect of reply : AsyncReplyChannel<bool>
        | SendCode of phone : string
        | MakeAuth of code : string
        | ResolveUsername of name : string * AsyncReplyChannel<(int * int64) option>
        | GetHistory of limit : int * id : (int * int64) * AsyncReplyChannel<{| messages : {| created : int; fromId : int option; id : int; message : string |} list; users : {| firstName : string; lastName : string; id : int |} list |}>

    let private resolveUsername (client : TelegramClient) name =
        async {
            let toInputPeerChannel (response : TLResolvedPeer) =
                let channel = response.Chats.[0] :?> TLChannel
                (channel.Id, channel.AccessHash.Value)
            let r = TLRequestResolveUsername(Username = name)
            let! r = client.SendRequestAsync<TLResolvedPeer>(r) 
            return Some (toInputPeerChannel r)
        }

    let create (sessionDir : string) : MailboxProcessor<Msg> =
        MailboxProcessor.Start(
            fun inbox ->
                async {
                    let mutable _client : TelegramClient = null
                    let mutable _phone = ""
                    let mutable _hash = ""
                    while true do
                        match! inbox.Receive() with
                        | ResetClient (appId, apiHash) -> 
                            let dir = IO.DirectoryInfo(sessionDir)
                            dir.Create()
                            _client <- new TelegramClient (appId, apiHash, FileSessionStore(dir))
                        | Connect reply ->
                            do! _client.ConnectAsync()    
                            reply.Reply <| _client.IsUserAuthorized()
                        | SendCode phone ->
                            let! hash = _client.SendCodeRequestAsync(phone)
                            _phone <- phone
                            _hash <- hash
                        | MakeAuth code ->
                            do! _client.MakeAuthAsync (_phone, _hash, code)
                        | ResolveUsername (name, reply) ->
                            match! resolveUsername _client name |> Async.Catch with
                            | Choice1Of2 id -> reply.Reply id
                            | Choice2Of2 e ->
                                printfn "ResolveUsername ERROR : %O" e
                                do! _client.ConnectAsync()
                                let! id = resolveUsername _client name
                                reply.Reply id
                        | GetHistory (limit, (id, accessHash), reply) ->
                            let p = TLInputPeerChannel(ChannelId = id, AccessHash = accessHash)
                            let! (r : Messages.TLAbsMessages) = _client.GetHistoryAsync(p, limit = limit) 
                            let channelMessages = r :?> Messages.TLChannelMessages
                            let users =
                                channelMessages.Users
                                |> Seq.map ^ fun x -> x :?> TLUser
                                |> Seq.map ^ fun x -> {| firstName = x.FirstName; lastName = x.LastName; id = x.Id |}
                                |> Seq.toList
                            let messages = 
                                channelMessages.Messages
                                |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
                                |> Seq.map ^ fun x -> {| created = x.Date; fromId = x.FromId |> Option.ofNullable; id = x.Id; message = x.Message |}
                                |> Seq.toList
                            reply.Reply <|  {| messages = messages; users = users |}
                }
        )

module Domain =
    type Snapshot = { message : string; author : string; id : string; created : DateTime }
    
    let parseString name (maxLength : int) (r : (string * string option) list) = 
        r
        |> Map.ofList
        |> Map.tryFind name
        |> Option.flatten
        |> Option.filter ^ fun x -> x.Length <= maxLength
        |> Result.ofOption ^ sprintf "not found %s" name

    let parseAuthForm r =
        let phone = parseString "phone" 32 r
        let appId = parseString "appId" 16 r |> Result.map int
        let apiHash = parseString "apiHash" 32 r
        Result.lift3 (fun a b c -> a, b, c) phone appId apiHash

    let private unixTimeStampToDateTime unixTimeStamp =
        let dtDateTime = DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)
        dtDateTime.AddSeconds (float unixTimeStamp)

    let toSnapshots (messages : {| messages : {| created : int; fromId : int option; id : int; message : string |} list; users : {| firstName : string; lastName : string; id : int |} list |}) =
        let users =
            messages.users
            |> Seq.map ^ fun x -> x.id, x
            |> Map.ofSeq
        messages.messages
        |> Seq.map ^ fun x ->
            { id = string x.id
              message = x.message
              created = unixTimeStampToDateTime x.created
              author = 
                x.fromId
                |> Option.bind ^ fun id -> Map.tryFind id users
                |> Option.map ^ fun x -> sprintf "%s %s" x.firstName x.lastName |> String.trim
                |> Option.defaultValue "<no name>" }
        |> Seq.rev
        |> Seq.toArray

module ResponseMessages =
    let connectResult succ =
        if succ then "Already authorized"
        else "Waiting for code"
    let cantFindChat x = sprintf "Can't find chat in %O" x
    let successAuthorized = "Success authorized"
    let toJsonResponse messages =
        messages
        |> Domain.toSnapshots
        |> System.Text.Json.JsonSerializer.Serialize

module Service =
    open Telegram

    let resetClient (telegram : MailboxProcessor<Msg>) r =
        async {
            let (phone, appId, apiHash) = Domain.parseAuthForm r |> Result.unwrap
            telegram.Post <| ResetClient (appId, apiHash)
            let! isConnected = telegram.PostAndAsyncReply Connect
            if not isConnected then 
                telegram.Post <| SendCode phone
            return ResponseMessages.connectResult isConnected
        }
    let login (telegram : MailboxProcessor<Msg>) r =
        async {
            let code = Domain.parseString "code" 8 r |> Result.unwrap
            telegram.Post <| MakeAuth code
            return ResponseMessages.successAuthorized
        }
    let getLastMessages (telegram : MailboxProcessor<Msg>) r =
        async {
            let username = Domain.parseString "chat" 32 r |> Result.unwrap
            let! response = telegram.PostAndAsyncReply (fun r -> ResolveUsername (username, r))
            let chatId = response |> Result.ofOption (ResponseMessages.cantFindChat response) |> Result.unwrap 
            let! history = telegram.PostAndAsyncReply (fun r -> GetHistory (25, chatId, r))
            return history |> ResponseMessages.toJsonResponse
        }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators

    let start password =
        let telegram = Telegram.create "__data"
        let resetClient = Service.resetClient telegram
        let login = Service.login telegram
        let getLastMessages = Service.getLastMessages telegram

        let runCommand g f = 
            request (fun r ctx -> 
                async {
                    let! result = f <| g r
                    return! Successful.OK result ctx 
                })
            >=> Writers.setMimeType "application/json"
        Authentication.authenticateBasic (fun (_, p) -> p = password) ^ choose [
            POST >=> path "/reset" >=> runCommand (fun r -> r.form) resetClient
            POST >=> path "/set-code" >=> runCommand (fun r -> r.form) login
            GET >=> path "/history" >=> runCommand (fun r -> r.query) getLastMessages ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    match args with 
    | [| pass; |] -> Server.start pass |> Async.RunSynchronously
    | _ -> printfn "telegram-rest <password>"
    0
