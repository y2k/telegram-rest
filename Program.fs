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
        member __.Bind(t : Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R>  = 
            async.Bind(Async.AwaitTask t, f)

type Snapshot = { title : string; author : string; uri : Uri }
type TelegramClient = NoneClient | RealClient of appId : string * apiHash : string * Lazy<TLSharp.Core.TelegramClient>
type TelegramStatus = { hash : string; phone : string; authorized : bool }
    with static member empty = { hash = ""; phone = ""; authorized = false }

type Global =
    { client : TelegramClient
      status : TelegramStatus
      chats : Map<string, TLInputPeerChannel>
      history : Map<string, Snapshot list> }
    with static member empty = { client = NoneClient; chats = Map.empty; status = TelegramStatus.empty; history = Map.empty }

type 'a Eff =
    | ConnectEff of (bool -> 'a -> 'a * 'a Eff)
    | SendCodeRequest of phone : string * (string -> 'a -> 'a * 'a Eff)
    | MakeAuthRequest of phone : string * hash : string * code : string
    | ResolveUsernamRequest of Contacts.TLRequestResolveUsername * (Contacts.TLResolvedPeer -> 'a -> 'a * 'a Eff)
    | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> 'a -> 'a * 'a Eff)
    | Terminate
    | TerminateWithError of string

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

        let isInt : Validation<string> = (Int32.TryParse >> fst), (sprintf "must be an int"), ("", "")
        let formDesc : Form<{| appId : string; apiHash : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.appId @>), [ isInt ])
                    TextProp ((fun f -> <@ f.apiHash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        match bindForm formDesc r with 
        | Choice1Of2 x -> 
            { db with client = RealClient (x.appId, x.apiHash, lazy(new TelegramClient(int x.appId, x.apiHash))) }, 
            ConnectEff ^ onConnected x.phone
        | Choice2Of2 e -> db, TerminateWithError e

    let login r (db : Global) =
        let formDesc : Form<{| code : string |}> =
            Form ([ TextProp ((fun f -> <@ f.code @>), [ maxLength 8 ]) ], [])
        match bindForm formDesc r  with
        | Choice1Of2 x -> db, MakeAuthRequest (db.status.phone, db.status.hash, x.code)
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
                            |> Option.defaultValue "<no name>"
                          uri = Uri <| sprintf "https://t.me/%s/%i" chatName x.Id }
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
    let resetClient r =
        (fun db -> sprintf "IsAuthorized: %b" db.status.authorized |> Text.Encoding.UTF8.GetBytes), 
        Domain.resetClient r

    let getHistory (uri : string) =
        let getHistoryFromDb db =
            match Domain.getChatId uri with
            | Error e -> failwith e
            | Ok chatName -> 
                Map.tryFind chatName db.history 
                |> Option.defaultValue []
                |> List.toArray
                |> Suave.Json.toJson
        getHistoryFromDb, (Domain.getHistory uri)

module Interpretator =
    let state = ref Global.empty

    let rec invoke terminate f =
        let client () = match (!state).client with RealClient (_,_,f) -> f.Value | NoneClient -> failwith "no client"
        async {
            let (newDb, eff) = f !state
            if !state = newDb 
                then printfn "LOG :: -in-> invoke() | eff = %O | db = %O" eff newDb
                else printfn "LOG :: -in-> invoke() | eff = %O | old = %O | new = %O" eff !state newDb
            state := newDb
            match eff with
            | ConnectEff f ->
                let client = client()
                do! client.ConnectAsync()
                let (newDb, eff) = f (client.IsUserAuthorized()) !state
                state := newDb
                return! invoke terminate (fun db -> db, eff)
            | SendCodeRequest (phone, f) -> 
                let client = client()
                let! code = client.SendCodeRequestAsync phone
                let (a, b) = f code !state
                state := a
                return! invoke terminate (fun db -> db, b)
            | MakeAuthRequest (p, h, c) -> 
                let client = client()
                do! client.MakeAuthAsync(p, h, c)
                return terminate !state
            | ResolveUsernamRequest (r, f) -> 
                let client = client()
                let! a = client.SendRequestAsync<TeleSharp.TL.Contacts.TLResolvedPeer>(r)
                let (b, c) = f a !state
                state := b
                return! invoke terminate (fun db -> db, c)
            | GetHistoryRequest (p, l, f) ->
                let client = client()
                let! a = client.GetHistoryAsync(p, limit = l)
                let (b, c) = f a !state
                state := b
                return! invoke terminate (fun db -> db, c)
            | Terminate -> 
                let result = terminate !state
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
        let reset = request ^ fun r ctx -> async {
            let! result = ServerDomain.resetClient r ||> Interpretator.invoke
            return! Successful.ok result ctx }
        let setCode = request ^ fun r ctx -> async {
            do! Domain.login r |> Interpretator.invoke ignore
            return! Successful.NO_CONTENT ctx }
        let simleRead (uri : string) = 
            request ^ fun _ ctx -> async {
                let! result = ServerDomain.getHistory uri ||> Interpretator.invoke
                return! Successful.ok result ctx }
            >=> Writers.setMimeType "application/json"

        choose [
            GET >=> pathScan "/read/%s" simleRead
            Authentication.authenticateBasic
                (fun (_, p) -> p = pass)
                ^ choose [
                    POST >=> path "/reset" >=> reset
                    POST >=> path "/set-code" >=> setCode ] ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    Server.start args.[0] |> Async.RunSynchronously
    0
