module TelegramRest

open System
open TeleSharp.TL

[<AutoOpen>]
module Prelude =
    let [<Obsolete>] TODO() = failwith "???"
    let inline flip f a b = f b a
    let inline always x _ = x
    let inline (^) f x = f x
    let inline (>>-) a f = async {
        let! x = a
        return f x }
    let (|Regex|_|) pattern input =
            let m = Text.RegularExpressions.Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None
    module Result =
        let unwrap = function Ok x -> x | Error e -> failwith e
        let wrap f = try f () |> Ok with e -> Error e
    type Microsoft.FSharp.Control.AsyncBuilder with
        member __.Bind (t : Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)
        member __.ReturnFrom (t : Threading.Tasks.Task<'T>) : Async<'T> = 
            async.ReturnFrom(Async.AwaitTask t)

type Snapshot = { title : string; author : string }
type TelegramClient = NoneClient | RealClient of appId : int * apiHash : string * Lazy<TLSharp.Core.TelegramClient>
type TelegramStatus = { hash : string; phone : string; authorized : bool; appId : int; apiHash : string }
    with static member empty = { hash = ""; phone = ""; authorized = false; appId = 0; apiHash = "" }

type Global =
    { client : TelegramClient
      status : TelegramStatus
      chats : Map<string, TLInputPeerChannel>
      history : Map<string, Snapshot list> }
    with static member empty = { client = NoneClient; chats = Map.empty; status = TelegramStatus.empty; history = Map.empty }

type Eff =
    | ConnectEff of (bool -> Global -> Global * Eff)
    | SendCodeRequest of phone : string * (string -> Global -> Global * Eff)
    | MakeAuthRequest of phone : string * hash : string * code : string * (TLUser -> Global -> Global * Eff)
    | ResolveUsernamRequest of Contacts.TLRequestResolveUsername * (Contacts.TLResolvedPeer -> Global -> Global * Eff)
    | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> Global -> Global * Eff)
    | Terminate
    | TerminateWithError of string
    // with static member map f = function 
    //       | ConnectEff x -> ConnectEff (fun r ->)
    //       // | SendCodeRequest of phone : string * (string -> 'a -> 'a * 'a Eff)
    //       // | MakeAuthRequest of phone : string * hash : string * code : string * (TLUser -> 'a -> 'a * 'a Eff)
    //       // | ResolveUsernamRequest of Contacts.TLRequestResolveUsername * (Contacts.TLResolvedPeer -> 'a -> 'a * 'a Eff)
    //       // | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> 'a -> 'a * 'a Eff)
    //       | Terminate -> Terminate
    //       | TerminateWithError e -> TerminateWithError e
    //       | _ -> TODO()

module Domain =
    open Suave.Form
    open TLSharp.Core
    open TeleSharp.TL.Contacts

    let resetClient r (db : Global) =
        let onConnected phone (isAuthorized : bool) db =
            let onTokenReceived hash db = 
                { db with status = { db.status with hash = hash; phone = phone } }, Terminate
            if isAuthorized 
                then { db with status = { db.status with authorized = isAuthorized } }, Terminate
                else { db with status = { db.status with authorized = isAuthorized } }, SendCodeRequest (phone, onTokenReceived)

        let formDesc : Form<{| phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        match bindForm formDesc r with 
        | Choice1Of2 x ->
            { db with client = RealClient (db.status.appId, db.status.apiHash, lazy(new TelegramClient(db.status.appId, db.status.apiHash))) }, 
            ConnectEff ^ onConnected x.phone
        | Choice2Of2 e -> db, TerminateWithError e

    let login r (db : Global) =
        let formDesc : Form<{| code : string |}> =
            Form ([ TextProp ((fun f -> <@ f.code @>), [ maxLength 8 ]) ], [])
        match bindForm formDesc r  with
        | Choice1Of2 x -> db, MakeAuthRequest (db.status.phone, db.status.hash, x.code, fun _ db -> db, Terminate)
        | Choice2Of2 e -> db, TerminateWithError e

    let getChatId = function
        | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
        | Regex "^([\\w\\d_]+)$" [ id ] -> Ok id
        | origin -> Error ^ sprintf "Can't find id from %s" origin

    let getHistory uri (db : Global) =
        match getChatId uri with
        | Error e -> db, TerminateWithError e
        | Ok chatName -> 
            let onMessagesLoaded (messages : Messages.TLAbsMessages) db =
                let history =
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
                    |> Seq.toList
                { db with history = Map.add chatName history db.history }, Terminate
            let onChatResolved (response : TLResolvedPeer) db =
                if Seq.isEmpty response.Chats 
                    then Error ^ sprintf "Can't find chat in %O" response
                    else
                        let channel = response.Chats.[0] :?> TLChannel
                        TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)
                        |> Ok
                |> function
                | Ok r -> db, GetHistoryRequest(r, 50, onMessagesLoaded)
                | Error e -> db, TerminateWithError e

            TLRequestResolveUsername(Username = chatName)
            |> fun r -> db, ResolveUsernamRequest (r, onChatResolved)

module ServerDomain =
    type ServerEff = 
        { action : Global -> Global * Eff
          query : Global -> byte[] }

    let resetClient r =
        { action = Domain.resetClient r
          query = fun db -> sprintf "IsAuthorized: %b" db.status.authorized |> Text.Encoding.UTF8.GetBytes }
    let setCode r = 
        { action = Domain.login r
          query = fun _ -> [||] }
    let getHistory (chat : string) =
        let getHistoryFromDb db =
            match Domain.getChatId chat with
            | Error e -> failwith e
            | Ok chatName -> 
                Map.tryFind chatName db.history 
                |> Option.defaultValue []
                |> List.toArray
                |> Suave.Json.toJson
        { action = Domain.getHistory chat
          query = getHistoryFromDb }

module Interpretator =
    let private state = ref Global.empty
    let update (f : Global -> Global) = state := f !state

    let rec invoke (serEff : ServerDomain.ServerEff) =
        let runClientEff f e = async {
            let client = match (!state).client with RealClient (_,_,f) -> f.Value | NoneClient -> failwith "no client"
            let! r = e client
            let (db', eff') = f r !state
            state := db'
            return! invoke { serEff with action = fun db -> db, eff' } }
        async {
            let (newDb, eff) = serEff.action !state
            if !state = newDb 
                then printfn "LOG :: -in-> invoke() | eff = %O | db = %O" eff newDb
                else printfn "LOG :: -in-> invoke() | eff = %O | old = %O | new = %O" eff !state newDb
            state := newDb
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
            | Terminate -> 
                let result = serEff.query !state
                printfn "LOG :: <-out- invoke() | state = %O | result = %O" !state result
                return result
            | TerminateWithError e -> 
                printfn "LOG :: ERROR :: <-e:out- invoke() | state = %O | result = %s" !state e
                return failwith e
        }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators

    let start pass =
        let runEff eff = request ^ fun r ctx -> async {
            let! result = eff r |> Interpretator.invoke
            return! Successful.ok result ctx }
        let simleRead (uri : string) = 
            request ^ fun _ ctx -> async {
                let! result = ServerDomain.getHistory uri |> Interpretator.invoke
                return! Successful.ok result ctx }
            >=> Writers.setMimeType "application/json"

        choose [
            GET >=> pathScan "/read/%s" simleRead
            Authentication.authenticateBasic (fun (_, p) -> p = pass) ^ choose [
                POST >=> path "/reset" >=> runEff ServerDomain.resetClient
                POST >=> path "/set-code" >=> runEff ServerDomain.setCode ] ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    match args with 
    | [| pass; appId; apiHash |] ->
        Interpretator.update ^ fun db -> { db with status = { db.status with appId = int appId; apiHash = apiHash } }
        Server.start pass |> Async.RunSynchronously
    | _ -> printfn "telegram-rest <password> <appId> <apiHash>"
    0
