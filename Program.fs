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
      status : TelegramStatus }
    with static member empty = { client = NoneClient; status = TelegramStatus.empty }

type Callback<'a, 't> = 'a -> Global -> Global * 't Eff

and 't Eff =
    | Terminate of 't
    | TerminateWithError of string
    | ConnectEff of Callback<bool, 't>
    | SendCodeRequest of phone : string * Callback<string, 't>
    | MakeAuthRequest of phone : string * hash : string * code : string * Callback<TLUser, 't>
    | ResolveUsernamRequest of Contacts.TLRequestResolveUsername * Callback<Contacts.TLResolvedPeer, 't>
    | GetHistoryRequest of TLAbsInputPeer * limit : int * Callback<Messages.TLAbsMessages, 't>

module Domain =
    open Suave.Form
    open TLSharp.Core
    open TeleSharp.TL.Contacts

    let resetClient r (db : Global) =
        let onConnected phone (isAuthorized : bool) db =
            let onTokenReceived hash db = 
                { db with status = { db.status with hash = hash; phone = phone } }, Terminate "Waiting for code"
            if isAuthorized 
                then { db with status = { db.status with authorized = isAuthorized } }, Terminate "Already authorized"
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
        | Choice1Of2 x -> db, MakeAuthRequest (db.status.phone, db.status.hash, x.code, fun _ db -> db, Terminate "Success authorized")
        | Choice2Of2 e -> db, TerminateWithError e

    let getChatId = function
        | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
        | Regex "^([\\w\\d_]+)$" [ id ] -> Ok id
        | origin -> Error ^ sprintf "Can't find valid id from %s" origin

    let onMessagesLoaded (messages : Messages.TLAbsMessages) db =
        let channelMessages = (messages :?> Messages.TLChannelMessages)
        let users =
            channelMessages.Users
            |> Seq.map ^ fun x -> x :?> TLUser
            |> Seq.map ^ fun x -> x.Id, x
            |> Map.ofSeq
        let history =
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
            |> Suave.Json.toJson
            |> System.Text.Encoding.UTF8.GetString
        db, Terminate history

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

    let getHistory (r : Suave.Http.HttpRequest) (db : Global) =
        r.queryParamOpt "chat"
        |> Option.bind snd
        |> Result.ofOption "No parameter <chat>"
        |> Result.bind getChatId
        |> function
           | Error e -> db, TerminateWithError e
           | Ok chatName -> 
               TLRequestResolveUsername(Username = chatName)
               |> fun r -> db, ResolveUsernamRequest (r, onChatResolved)

module Interpretator =
    let private state = ref Global.empty
    let update (f : Global -> Global) = state := f !state

    let rec invoke (serEffFunc : Global -> Global * string Eff) : string Async =
        let runClientEff f e = async {
            let client = match (!state).client with RealClient (_,_,f) -> f.Value | NoneClient -> failwith "no client"
            let! r = e client
            let (db', eff') = f r !state
            state := db'
            return! invoke (fun db -> db, eff') }
        async {
            let (newDb, eff) = serEffFunc !state
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
            | Terminate result -> 
                printfn "LOG :: <-out- invoke() | state = %O | result = %O" !state result
                return result
            | TerminateWithError e -> 
                eprintfn "LOG :: ERROR :: <-e:out- invoke() | state = %O | result = %s" !state e
                return failwith e
        }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators

    let start pass =
        let runEff eff = 
            request ^ fun r ctx -> async {
                let! result = eff r |> Interpretator.invoke
                return! Successful.OK result ctx }
            >=> Writers.setMimeType "application/json"
        choose [
            GET >=> path "/history" >=> runEff Domain.getHistory
            Authentication.authenticateBasic (fun (_, p) -> p = pass) ^ choose [
                POST >=> path "/reset" >=> runEff Domain.resetClient
                POST >=> path "/set-code" >=> runEff Domain.login ] ]
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
