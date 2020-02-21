﻿module TelegramRest

module Foo' =
    type RawMessages = RawMessages
    type Model = Model
    type TLUser = TLUser
    type 't IEffect =
        abstract member invoke: Async<'t>
    
    type 't ResultEffect =
        { result : 't }
        interface 't IEffect with 
            member this.invoke : Async<'t> = this.result |> async.Return
    type 't MappedEffect =
        { origin : obj
          f : 't Async }
        interface 't IEffect with
            member this.invoke : Async<'t> = this.f
    module Effect =
        let lift x = { result = x } :> IEffect<_>
        let bind (f : 'a -> _ :> 'b IEffect) (origin : 'a IEffect) : 'b IEffect =
            let e =
                { MappedEffect.origin = origin
                  f = async {
                      let! a = origin.invoke
                      let b = f a
                      return! b.invoke
                  } 
                }
            e :> 'b IEffect
        let map f x = bind (f >> lift) x

    type ResolveName = 
        { name : string }
        interface TLUser IEffect with 
            member __.invoke : Async<TLUser> = failwith "???"
    type LoadHistory = 
        { user : TLUser; limit : int }
        interface RawMessages IEffect with 
            member __.invoke : Async<RawMessages> = failwith "???"

    let rawMessagesToResult (_ : RawMessages) : string = failwith "???"
    let tryFindUserName _ : Result<_,_> = failwith "???"

    let onChatHistoryRequested chat =
        match tryFindUserName chat with
        | Error e -> Effect.lift e
        | Ok chat ->
            { ResolveName.name = chat } 
            |> Effect.bind ^ fun user -> { LoadHistory.user = user; limit = 50 }
            |> Effect.map rawMessagesToResult

    let onChatHistoryRequested'' chat : IEffect<string> =
        let onChatResolved (user : TLUser) : IEffect<string> = 
            let ofHistoryLoaded (message : RawMessages) : IEffect<string> = 
                Effect.lift (string message)
        
            { LoadHistory.user = user; limit = 50 }
            |> Effect.bind ofHistoryLoaded
    
        { ResolveName.name = chat } 
        |> Effect.bind onChatResolved

module Foo =
    type Model = Model
    type TLUser = TLUser
    type IEffect =
        abstract member invoke: Async<unit>
    
    type ResolveName = 
        { user : string; next : TLUser -> Model -> Model * IEffect }
        interface IEffect with 
            member __.invoke : Async<unit> = failwith "???"
    type LoadHistory = 
        { user : TLUser; next : unit list -> Model -> Model * IEffect }
        interface IEffect with 
            member __.invoke : Async<unit> = failwith "???"

    let doSomethingWithModel3 x = x
    let onChatResolved (user : TLUser) (model : Model) : Model * IEffect = 
        failwith "???"
    let onChatHistoryRequested chat model : Model * IEffect =
        doSomethingWithModel3 model, 
        { user = chat; ResolveName.next = onChatResolved } :> IEffect

open System

[<AutoOpen>]
module Prelude =
    let inline ignoreDb f x db = db, f x

module Types =
    type Snapshot = { title : string; author : string }
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
            { title = x.Message
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

    let parseCode r =
        let formDesc : Form<{| code : string |}> =
            Form ([ TextProp ((fun f -> <@ f.code @>), [ maxLength 8 ]) ], [])
        bindForm formDesc r

    let parsePhone r =
        let formDesc : Form<{| phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        bindForm formDesc r

    let toInputPeerChannel (response : TLResolvedPeer) =
        let channel = response.Chats.[0] :?> TLChannel
        TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)

module ResponseDomain =
    let waitingForCode = "Waiting for code"
    let alreadyAuthorized = "Already authorized"
    let cantFindChat x = sprintf "Can't find chat in %O" x
    let successAuthorized = "Success authorized"
    let toJsonResponse messages =
        messages
        |> Suave.Json.toJson
        |> Text.Encoding.UTF8.GetString

let Global = Atom.atom Types.Global.empty

module Services =
    open Types
    open Domain
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    type Callback<'a, 't> = 'a -> Types.Global -> Types.Global * 't Command
    and 't Command =
        | Terminate of 't
        | TerminateWithError of string
        | ConnectEff of Callback<bool, 't>
        | SendCodeRequest of phone : string * Callback<string, 't>
        | MakeAuthRequest of phone : string * hash : string * code : string * Callback<TLUser, 't>
        | ResolveUsernamRequest of TLRequestResolveUsername * Callback<TLResolvedPeer, 't>
        | GetHistoryRequest of TLAbsInputPeer * limit : int * Callback<Messages.TLAbsMessages, 't>

    module Test =
        type 't Cmd =
            | ConnectEff of (bool -> 't)
            | SendCodeRequest of phone : string * (string -> 't)
            | MakeAuthRequest of phone : string * hash : string * code : string * (TLUser -> 't)
            | ResolveUsernamRequest of TLRequestResolveUsername * (TLResolvedPeer -> 't)
            | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> 't)
        type 't Step =
            | Terminate of 't
            | Next of 't Step Cmd
    
        let onTokenReceived (phone : string) (hash : string) =
            // { db with status = { db.status with hash = hash; phone = phone } }, 
            Terminate ResponseDomain.waitingForCode
        let onConnected (phone : string) isAuthorized =
            match isAuthorized with
            | true -> Terminate ResponseDomain.alreadyAuthorized
            | false -> Next (SendCodeRequest (phone, onTokenReceived phone))
        let resetClient r db =
            match parsePhone r with 
            | Choice1Of2 x ->
                // { db with client = TelegramClient.create db.status.appId db.status.apiHash }, 
                Next (ConnectEff (onConnected x.phone))
            | Choice2Of2 e -> Terminate (string e)
        
    let resetClient r db =
        let onConnected phone isAuthorized =
            let onTokenReceived phone hash db = 
                { db with status = { db.status with hash = hash; phone = phone } }, 
                Terminate ResponseDomain.waitingForCode
        
            match isAuthorized with
            | true -> Terminate ResponseDomain.alreadyAuthorized
            | false -> SendCodeRequest (phone, onTokenReceived phone)
    
        match parsePhone r with 
        | Choice1Of2 x ->
            { db with client = TelegramClient.create db.status.appId db.status.apiHash }, 
            ConnectEff ^ ignoreDb (onConnected x.phone)
        | Choice2Of2 e -> db, TerminateWithError e

    let login r db =
        match parseCode r with
        | Choice1Of2 x -> db, MakeAuthRequest (db.status.phone, db.status.hash, x.code, fun _ db -> db, Terminate ResponseDomain.successAuthorized)
        | Choice2Of2 e -> db, TerminateWithError e

    let getHistory r =
        let onChatResolved (response : TLResolvedPeer) =
            let onMessagesLoaded messages =
                toSnapshots messages
                |> ResponseDomain.toJsonResponse
                |> Terminate 
        
            match Seq.isEmpty response.Chats with
            | true -> TerminateWithError ^ ResponseDomain.cantFindChat response
            | false ->
                let ch = toInputPeerChannel response
                GetHistoryRequest(ch, 50, ignoreDb onMessagesLoaded)

        match parseChat r with
        | Error e -> TerminateWithError e
        | Ok name -> 
            ResolveUsernamRequest (TLRequestResolveUsername(Username = name), ignoreDb onChatResolved)

    module Interpretator =
        let rec invoke (update : Global -> Global * 'a Command) : 'a Async =
            let runClientEff f e = 
                async {
                    let client = (Global.Value).client |> TelegramClient.unwrap
                    let! r = e client
                    let newEff = Global.dispatch ^ fun db -> f r db
                    return! invoke (fun db -> db, newEff) 
                }
            async {
                let oldState = Global.Value
                let eff = Global.dispatch update
                if Global.Value = oldState 
                    then printfn "LOG :: -in-> invoke() | eff = %O | db = %O" eff oldState
                    else printfn "LOG :: -in-> invoke() | eff = %O | old = %O | new = %O" eff oldState Global.Value
                match eff with
                | ConnectEff f ->
                    return! runClientEff f ^ fun client -> async {
                        do! client.ConnectAsync()    
                        return client.IsUserAuthorized() }
                | SendCodeRequest (phone, f) ->
                    return! runClientEff f ^ fun client -> async { return! client.SendCodeRequestAsync phone }
                | MakeAuthRequest (p, h, c, f) ->
                    return! runClientEff f ^ fun client -> async { return! client.MakeAuthAsync(p, h, c) }
                | ResolveUsernamRequest (r, f) -> 
                    return! runClientEff f ^ fun client -> async { return! client.SendRequestAsync<Contacts.TLResolvedPeer>(r) }
                | GetHistoryRequest (p, l, f) ->
                    return! runClientEff f ^ fun client -> async { return! client.GetHistoryAsync(p, limit = l) }
                | Terminate result -> 
                    printfn "LOG :: <-out- invoke() | state = %O | result = %O" Global.Value result
                    return result
                | TerminateWithError e -> 
                    eprintfn "LOG :: ERROR :: <-e:out- invoke() | state = %O | result = %s" Global.Value e
                    return failwith e
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
        Global.update ^ fun db -> { db with status = { db.status with appId = int appId; apiHash = apiHash } }
        Server.start pass |> Async.RunSynchronously
    | _ -> printfn "telegram-rest <password> <appId> <apiHash>"
    0
