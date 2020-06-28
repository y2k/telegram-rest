﻿module TelegramRest

open System

module Telegram =
    open TLSharp.Core
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    type History = {| messages : {| created : int; fromId : int option; id : int; message : string |} list; users : {| firstName : string; lastName : string; id : int |} list |}

    type Msg = 
        | ResetClient of appId : int * apiHash : string
        | Connect of reply : AsyncReplyChannel<bool>
        | SendCode of phone : string
        | MakeAuth of code : string
        | ResolveUsername of name : string * AsyncReplyChannel<(int * int64 option) option>
        | GetHistory of limit : int * id : (int * int64 option) * AsyncReplyChannel<History>

    let private resolveUsername (client : TelegramClient) name =
        let toInputPeerChannel (response : TLResolvedPeer) =
            let nullableToOption (n : Nullable<_>) = 
               if n.HasValue then Some n.Value else None
            let channel = response.Chats.[0] :?> TLChannel
            (channel.Id, nullableToOption channel.AccessHash)
        async {
            let r = TLRequestResolveUsername(Username = name)
            let! r = client.SendRequestAsync<TLResolvedPeer>(r) |> Async.AwaitTask |> Async.Catch
            return
                match r with
                | Choice1Of2 r -> Some (toInputPeerChannel r)
                | Choice2Of2 (:? AggregateException as e) when e.InnerException.Message = "USERNAME_NOT_OCCUPIED" -> None
                | Choice2Of2 e -> raise e
        }

    let private getHistory (client : TelegramClient) id accessHash limit =
        async {
            let p = 
                match accessHash with
                | Some accessHash -> TLInputPeerChannel(ChannelId = id, AccessHash = accessHash)
                | None -> TLInputPeerChannel(ChannelId = id)
            let! (r : Messages.TLAbsMessages) = client.GetHistoryAsync(p, limit = limit) 
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
            return {| messages = messages; users = users |}
        }

    type State = 
        { mutable client : TelegramClient
          mutable phone : string
          mutable hash : string
          mutable appId : int
          mutable apiHash : string
          mutable getHistoryCount : int64
          mutable recreateCount : int64
          dir : IO.DirectoryInfo }

    let recreateClient e state =
        state.recreateCount <- state.recreateCount + 1L
        printfn "%O :: RecreateClient (count = %i, IsConnected = %b) because %O" DateTime.Now state.recreateCount state.client.IsConnected e
        state.client.Dispose()
        state.client <- new TelegramClient (state.appId, state.apiHash, FileSessionStore(state.dir))
        state.client.ConnectAsync()

    let private handleMessage msg (state : State) =
        async {
            match msg with
            | ResetClient (appId, apiHash) -> 
                state.appId <- appId
                state.apiHash <- apiHash
                state.client <- new TelegramClient (appId, apiHash, FileSessionStore(state.dir))
            | Connect reply ->
                do! state.client.ConnectAsync()    
                reply.Reply <| state.client.IsUserAuthorized()
            | SendCode phone ->
                let! hash = state.client.SendCodeRequestAsync(phone)
                state.phone <- phone
                state.hash <- hash
            | MakeAuth code ->
                do! state.client.MakeAuthAsync (state.phone, state.hash, code)
            | ResolveUsername (name, reply) ->
                match! resolveUsername state.client name |> Async.Catch with
                | Choice1Of2 id ->
                    reply.Reply id
                | Choice2Of2 e ->
                    do! recreateClient e state
                    let! id = resolveUsername state.client name
                    reply.Reply id
            | GetHistory (limit, (id, accessHash), reply) ->
                state.getHistoryCount <- state.getHistoryCount + 1L
                printfn "%O :: GetHistory (count = %O, IsConnected = %b)" DateTime.Now state.getHistoryCount state.client.IsConnected
                match! getHistory state.client id accessHash limit |> Async.Catch with
                | Choice1Of2 history -> reply.Reply history
                | Choice2Of2 e ->
                    do! recreateClient e state
                    let! history = getHistory state.client id accessHash limit
                    reply.Reply history
        }

    let create (sessionDir : string) : MailboxProcessor<Msg> =
        let dir = IO.DirectoryInfo(sessionDir)
        dir.Create()
        MailboxProcessor.Start(
            fun inbox ->
                async {
                    let state = { client  = null; phone = ""; hash = ""; appId = 0; apiHash = ""; getHistoryCount = 0L; recreateCount = 0L; dir = dir }
                    while true do
                        let! msg = inbox.Receive()
                        try 
                            printfn "%O :: LOG-IN = %O" DateTime.Now msg
                            do! handleMessage msg state
                            printfn "%O :: LOG-OUT = %O" DateTime.Now msg
                        with e -> eprintfn "%O :: ERROR (%O): %O" DateTime.Now msg e
                })

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

module Cache =
    open System.Runtime.Caching
    type 't t = { cache : MemoryCache; livetime : TimeSpan }
    type 't entry = { item : 't }
    let create livetime = { cache = new MemoryCache ("cache"); livetime = livetime }
    let get (t : 't t) key : 't option =
        let r = t.cache.Get key
        if isNull r then None
        else (r :?> entry<'t>).item |> Some
    let set (t : 't t) key (value : 't) =
        t.cache.Set (key, { item = value}, DateTimeOffset.op_Implicit (DateTime.Now + t.livetime))

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
        let userIdCache : Cache.t<(int * int64 option) option> = Cache.create <| TimeSpan.FromHours 1.
        let historyCache : string Cache.t = Cache.create <| TimeSpan.FromMinutes 2.
        async {
            let username = Domain.parseString "chat" 32 r |> Result.unwrap
            let! response = 
                match Cache.get userIdCache username with
                | Some id -> 
                    printfn "%O :: get username from cache for %s" DateTime.Now username
                    async.Return id
                | None ->
                    async {
                        let! r = telegram.PostAndAsyncReply (fun r -> ResolveUsername (username, r))
                        Cache.set userIdCache username r
                        return r
                    }
            let chatId = response |> Result.ofOption (ResponseMessages.cantFindChat response) |> Result.unwrap 
            match Cache.get historyCache username with
            | Some history -> 
                printfn "%O :: return history from cache for %s" DateTime.Now username
                return history
            | None ->
                let! history = telegram.PostAndAsyncReply (fun r -> GetHistory (25, chatId, r))
                let response = history |> ResponseMessages.toJsonResponse
                Cache.set historyCache username response
                return response
        }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators

    let start password =
        let telegram = Telegram.create "__data"
        let getLastMessages = Service.getLastMessages telegram
        let runCommand g f = 
            request (fun r ctx -> 
                async {
                    let! result = f <| g r
                    return! Successful.OK result ctx 
                })
            >=> Writers.setMimeType "application/json"
        Authentication.authenticateBasic (fun (_, p) -> p = password) ^ choose [
            POST >=> path "/reset" >=> runCommand (fun r -> r.form) (Service.resetClient telegram)
            POST >=> path "/set-code" >=> runCommand (fun r -> r.form) (Service.login telegram)
            GET >=> path "/history" >=> runCommand (fun r -> r.query) getLastMessages ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    match args with 
    | [| pass; |] -> Server.start pass |> Async.RunSynchronously
    | _ -> printfn "telegram-rest <password>"
    0
