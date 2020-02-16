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

    module Result =
        let unwrap = function Ok x -> x | Error e -> failwith e
        let wrap f = try f () |> Ok with e -> Error e

    type Microsoft.FSharp.Control.AsyncBuilder with
        member __.Bind(t : Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R>  = 
            async.Bind(Async.AwaitTask t, f)

type Snapshot = { title : string; uri : Uri }
type TelegramClient = NoneClient | RealClient of Lazy<TLSharp.Core.TelegramClient>
type TelegramStatus = { code : string }

type Global =
    { client : TelegramClient
      status : TelegramStatus
      chats : Map<string, TLInputPeerChannel>
      history : Map<string, Snapshot list> }
    with static member empty = { client = NoneClient; chats = Map.empty; status = { code = "" }; history = Map.empty }

type 'a Eff =
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
        let onTokenReceived code db = 
            { db with status = { db.status with code = code } }, Terminate

        let isInt : Validation<string> = (Int32.TryParse >> fst), (sprintf "must be an int"), ("", "")
        let formDesc : Form<{| appId : string; apiHash : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.appId @>), [ isInt ])
                    TextProp ((fun f -> <@ f.apiHash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])

        match bindForm formDesc r with 
        | Choice1Of2 x -> 
            let client = RealClient ^ lazy(new TelegramClient(int x.appId, x.apiHash))
            { db with client = client }, 
            SendCodeRequest (x.phone, onTokenReceived)
        | Choice2Of2 e -> db, TerminateWithError e

    let resetClient' r =
        (fun db -> sprintf "Code: %s" db.status.code |> Text.Encoding.UTF8.GetBytes), 
        resetClient r

    let login r (db : Global) =
        let formDesc : Form<{| hash : string; code : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.hash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.code @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        match bindForm formDesc r  with
        | Choice1Of2 x -> db, MakeAuthRequest (x.phone, x.hash, x.code)
        | Choice2Of2 e -> db, TerminateWithError e

    let getHistory uri (db : Global) =
        match Result.wrap (fun _ -> (Uri uri).Segments.[1]) with
        | Error e -> db, TerminateWithError ^ sprintf "Error parse url: %s\n%O" uri e
        | Ok chatName -> 
            let onMessagesLoaded (messages : Messages.TLAbsMessages) db =
                let history =
                    (messages :?> Messages.TLChannelMessages).Messages
                    |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
                    |> Seq.map ^ fun x ->
                        { title = x.Message
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

    let getHistory' (uri : string) =
        let chatName = Uri uri |> fun uri -> uri.Segments.[1]
        let getHistoryFromDb db =
            Map.tryFind chatName db.history 
            |> Option.defaultValue []
            |> Suave.Json.toJson
        getHistoryFromDb, (getHistory uri)

module Interpretator =
    let state = ref Global.empty

    let rec invoke terminate f =
        async {
            let (newDb, eff) = f !state
            printfn "LOG :: -in-> invoke() | eff = %O | old = %O | new = %O" eff !state newDb
            state := newDb
            match eff with
            | SendCodeRequest (phone, f) -> 
                let client = match (!state).client with RealClient f -> f.Value | NoneClient -> failwith "no client"
                let! code = client.SendCodeRequestAsync phone
                let (a, b) = f code !state
                state := a
                return! invoke terminate (fun db -> db, b)
            | MakeAuthRequest (p, h, c) -> 
                let client = match (!state).client with RealClient f -> f.Value | NoneClient -> failwith "no client"
                do! client.MakeAuthAsync(p, h, c)
                return terminate !state
            | ResolveUsernamRequest (r, f) -> 
                let client = match (!state).client with RealClient f -> f.Value | NoneClient -> failwith "no client"
                let! a = client.SendRequestAsync<TeleSharp.TL.Contacts.TLResolvedPeer>(r)
                let (b, c) = f a !state
                state := b
                return! invoke terminate (fun db -> db, c)
            | GetHistoryRequest (p, l, f) ->
                let client = match (!state).client with RealClient f -> f.Value | NoneClient -> failwith "no client"
                let! a = client.GetHistoryAsync(p, 0, 0, l)
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
            let! result = Domain.resetClient' r ||> Interpretator.invoke
            return! Successful.ok result ctx }
        let setCode = request ^ fun r ctx -> async {
            do! Domain.login r |> Interpretator.invoke ignore
            return! Successful.NO_CONTENT ctx }
        let simleRead (uri : string) = request ^ fun _ ctx -> async {
            let! result = Domain.getHistory' uri ||> Interpretator.invoke
            return! Successful.ok result ctx }

        choose [
            POST >=> pathScan "/simple-read/%s" simleRead
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
