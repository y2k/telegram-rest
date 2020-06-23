module TelegramRest

open System

[<AutoOpen>]
module Prelude =
    let inline ignoreDb f x db = db, f x

module Types =
    type Snapshot = { title : string; author : string; id : string }
    type TelegramClient = NoneClient | RealClient of appId : int * apiHash : string * Lazy<TLSharp.Core.TelegramClient>
        with static member unwrap client = match client with RealClient (_,_,f) -> f.Value | NoneClient -> failwith "no client"
             static member create appId apiHash = RealClient (appId, apiHash, lazy(new TLSharp.Core.TelegramClient(appId, apiHash)))
    type TelegramStatus = { hash : string; phone : string; appId : int; apiHash : string }
        with static member empty = { hash = ""; phone = ""; appId = 0; apiHash = "" }
    type Global = { client : TelegramClient; status : TelegramStatus }
        with static member empty = { client = NoneClient; status = TelegramStatus.empty }

module Domain =
    open Types
    open Suave.Form
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    let toSnapshots (messages : Messages.TLAbsMessages) =
        let channelMessages = (messages :?> Messages.TLChannelMessages)
        let users =
            channelMessages.Users
            |> Seq.map ^ fun x -> x :?> TLUser
            |> Seq.map ^ fun x -> x.Id, x
            |> Map.ofSeq
        channelMessages.Messages
        |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
        |> Seq.map ^ fun x ->
            { id = string x.Id
              title = x.Message
              author = 
                x.FromId
                |> Option.ofNullable
                |> Option.bind ^ fun id -> Map.tryFind id users
                |> Option.map ^ fun x -> sprintf "%s %s (%i)" x.FirstName x.LastName x.Id
                |> Option.defaultValue "<no name>" }
        |> Seq.rev
        |> Seq.toArray

    let getChatId = function
        | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
        | Regex "^([\\w\\d_]+)$" [ id ] -> Ok id
        | origin -> Error ^ sprintf "Can't find valid id from %s" origin

    let parseChat (r : Suave.Http.HttpRequest) =
        r.queryParamOpt "chat"
        |> Option.bind snd
        |> Result.ofOption "No parameter <chat>"
        |> Result.bind getChatId

    let parseString len r =
        let formDesc : Form<{| value : string |}> =
            Form ([ TextProp ((fun f -> <@ f.value @>), [ maxLength len ]) ], [])
        bindForm formDesc r |> Choice.map (fun x -> x.value) |> Result.ofChoice

    let toInputPeerChannel (response : TLResolvedPeer) =
        let channel = response.Chats.[0] :?> TLChannel
        TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)

module ResponseMessages =
    let waitingForCode = "Waiting for code"
    let alreadyAuthorized = "Already authorized"
    let cantFindChat x = sprintf "Can't find chat in %O" x
    let successAuthorized = "Success authorized"
    let toJsonResponse messages =
        messages
        |> Suave.Json.toJson
        |> Text.Encoding.UTF8.GetString

module Services =
    open Types
    open Domain
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    type 't Cmd =
        | ConnectEff of (bool -> Global -> Global * 't)
        | SendCodeRequest of value : string * (string -> Global -> Global * 't)
        | MakeAuthRequest of value : string * hash : string * code : string * (TLUser -> Global -> Global * 't)
        | ResolveUsernamRequest of TLRequestResolveUsername * (TLResolvedPeer -> Global -> Global * 't)
        | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> Global -> Global * 't)
    type 't Next =
        | Terminate of Result<'t, string>
        | Next of 't Next Cmd

    let resetClient r db =
        let onConnected phone isAuthorized =
            let onTokenReceived hash db = 
                { db with status = { db.status with hash = hash } }, 
                Terminate ^ Ok ResponseMessages.waitingForCode

            match isAuthorized with
            | true -> Terminate ^ Ok ResponseMessages.alreadyAuthorized
            | false -> Next ^ SendCodeRequest (phone, onTokenReceived)

        match parseString 32 r with 
        | Ok phone ->
            { db with status = { db.status with phone = phone }; client = TelegramClient.create db.status.appId db.status.apiHash }, 
            Next ^ ConnectEff ^ ignoreDb (onConnected phone)
        | Error e -> db, Terminate ^ Error e

    let login r db =
        let onLoginEnd _ db = db, Terminate ^ Ok ResponseMessages.successAuthorized
        db
        , match parseString 8 r with
          | Ok code -> Next ^ MakeAuthRequest (db.status.phone, db.status.hash, code, onLoginEnd)
          | Error e -> Terminate ^ Error e

    let getHistory r =
        let onChatResolved (response : TLResolvedPeer) =
            let onMessagesLoaded messages =
                toSnapshots messages
                |> ResponseMessages.toJsonResponse
                |> (Ok >> Terminate)

            match Seq.isEmpty response.Chats with
            | true -> Terminate ^ Error ^ ResponseMessages.cantFindChat response
            | false ->
                let ch = toInputPeerChannel response
                Next ^ GetHistoryRequest(ch, 50, ignoreDb onMessagesLoaded)

        match parseChat r with
        | Error e -> Terminate ^ Error e
        | Ok name -> 
            Next ^ ResolveUsernamRequest (TLRequestResolveUsername(Username = name), ignoreDb onChatResolved)

    module Interpretator =
        let executeEff eff (client : TLSharp.Core.TelegramClient) =
            async {
                match eff with
                | ConnectEff f ->
                    do! client.ConnectAsync()    
                    let x = client.IsUserAuthorized() 
                    return f x
                | SendCodeRequest (phone, f) ->
                    let! x = client.SendCodeRequestAsync(phone)
                    return f x
                | MakeAuthRequest (p, h, c, f) ->
                    let! x = client.MakeAuthAsync(p, h, c) 
                    return f x
                | ResolveUsernamRequest (r, f) -> 
                    let! x = client.SendRequestAsync<TLResolvedPeer>(r) 
                    return f x
                | GetHistoryRequest (p, l, f) ->
                    let! x = client.GetHistoryAsync(p, limit = l) 
                    return f x
            }

        let store = Atom.atom Global.empty

        let rec invoke update : 'a Async =
            async {
                let oldState = store.Value
                let eff = store.dispatch update
                if store.Value = oldState 
                    then printfn "LOG :: -in-> invoke() | eff = %O | db = %O" eff oldState
                    else printfn "LOG :: -in-> invoke() | eff = %O | old = %O | new = %O" eff oldState store.Value
                match eff with
                | Terminate (Ok result) ->
                    printfn "LOG :: <-out- invoke() | state = %O | result = %O" store.Value result
                    return result
                | Terminate (Error e) -> 
                    eprintfn "LOG :: ERROR :: <-e:out- invoke() | state = %O | result = %s" store.Value e
                    return failwith e
                | Next eff -> 
                    let client = (store.Value).client |> TelegramClient.unwrap
                    let! f = executeEff eff client
                    let newEff = store.dispatch ^ fun db -> f db
                    return! invoke (fun db -> db, newEff) 
            }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators

    let start pass =
        let runEff eff = 
            request ^ fun r ctx -> async {
                let! result = eff r |> Services.Interpretator.invoke
                return! Successful.OK result ctx }
            >=> Writers.setMimeType "application/json"

        Authentication.authenticateBasic (fun (_, p) -> p = pass) ^ choose [
            GET >=> path "/history" >=> runEff ^ ignoreDb Services.getHistory
            POST >=> path "/reset" >=> runEff Services.resetClient
            POST >=> path "/set-code" >=> runEff Services.login ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    match args with 
    | [| pass; appId; apiHash |] ->
        Services.Interpretator.store.update ^ fun db -> 
            { db with status = { db.status with appId = int appId; apiHash = apiHash } }
        Server.start pass |> Async.RunSynchronously
    | _ -> printfn "telegram-rest <password> <appId> <apiHash>"
    0
