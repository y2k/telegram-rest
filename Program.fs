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

    type Microsoft.FSharp.Control.AsyncBuilder with
        member __.Bind(t : Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R>  = 
            async.Bind(Async.AwaitTask t, f)

type Snapshot = { title : string; uri : Uri }
type TelegramClient = NoneClient | RealClient of (unit -> TLSharp.Core.TelegramClient)
type TelegramStatus = { isAuthorized : bool; code : string }

type Global =
    { client : TelegramClient
      status : TelegramStatus
      chats : Map<string, TLInputPeerChannel>
      history : Map<string, Snapshot list> }
    with static member empty = { client = NoneClient; chats = Map.empty; status = { isAuthorized = false; code = "" }; history = Map.empty }

type TelegramEff =
    | SendCodeRequest of phone : string * (string -> Global -> Global)
    | MakeAuthRequest of phone : string * hash : string * code : string
    | ResolveUsernamRequest of Contacts.TLRequestResolveUsername * (Contacts.TLResolvedPeer -> Global -> Result<Global, string>)
    | GetHistoryRequest of TLAbsInputPeer * limit : int * (Messages.TLAbsMessages -> Global -> Global)

type Eff =
    | UpdateGlobalEff of (Global -> Global)
    | TelegramEff of TelegramEff

type Next =
    | Next of Eff * (Global -> Next) 
    | Terminate 
    | TerminateWithError of string

module Domain =
    open Suave.Form
    open TLSharp.Core
    open TeleSharp.TL.Contacts

    let resetClient r (_ : Global) =
        let clientUpdated phone (state : Global) =
            if state.status.isAuthorized 
                then Terminate
                else 
                    SendCodeRequest (phone, fun code db -> { db with status = { db.status with code = code }})
                    |> TelegramEff
                    |> fun eff -> Next (eff, fun _ -> Terminate)

        let isInt : Validation<string> = (Int32.TryParse >> fst), (sprintf "must be an int"), ("", "")
        let formDesc : Form<{| appId : string; apiHash : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.appId @>), [ isInt ])
                    TextProp ((fun f -> <@ f.apiHash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        match bindForm formDesc r with 
        | Choice1Of2 x -> 
            let client = RealClient ^ fun _ -> new TelegramClient(int x.appId, x.apiHash)
            let eff = UpdateGlobalEff ^ fun db -> { db with client = client }
            Next (eff, clientUpdated x.phone)
        | Choice2Of2 e -> TerminateWithError e

    let resetClient' r =
        (fun db -> sprintf "Code: %s" db.status.code), resetClient r

    let login r (_ : Global) =
        let formDesc : Form<{| hash : string; code : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.hash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.code @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        match bindForm formDesc r  with
        | Choice1Of2 x ->
            let eff =
                MakeAuthRequest (x.phone, x.hash, x.code)
                |> TelegramEff
            Next (eff, fun _ -> Terminate)
        | Choice2Of2 e -> TerminateWithError e

    let getHistory chatName (_ : Global) = 
        let saveChat (response : TLResolvedPeer) db = 
            if Seq.isEmpty response.Chats 
                then Error ^ sprintf "Can't find chat in %O" response
                else
                    let channel = response.Chats.[0] :?> TLChannel
                    TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)
                    |> Ok
            |> Result.map ^ fun r -> { db with chats = Map.add chatName r db.chats }

        let saveHistory (db : Global) =
            let parse (messages : Messages.TLAbsMessages) db =
                let history =
                    (messages :?> Messages.TLChannelMessages).Messages
                    |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
                    |> Seq.map ^ fun x ->
                        { title = x.Message
                          uri = Uri <| sprintf "https://t.me/%s/%i" chatName x.Id }
                    |> Seq.rev
                    |> Seq.toList
                { db with history = Map.add chatName history db.history }
            let r = Map.find chatName db.chats
            GetHistoryRequest (r, 50, parse)
            |> TelegramEff
            |> fun eff -> Next (eff, fun _ -> Terminate)
        
        TLRequestResolveUsername(Username = chatName)
        |> fun r -> ResolveUsernamRequest (r, saveChat)
        |> TelegramEff
        |> fun eff -> Next (eff, saveHistory)

    let getHistory' (uri : string) =
        let chatName = Uri uri |> fun uri -> uri.Segments.[1]
        let getHistoryFromDb db =
            Map.tryFind chatName db.history 
            |> Option.defaultValue []
        getHistoryFromDb, (getHistory chatName)

module Interpretator =
    let state = ref Global.empty

    let rec invoke (terminate : Global -> 'a) (f : Global -> Next) : 'a Async =
        async {
            printfn "LOG :: -in-> invoke() | state = %O" !state
            match f !state with
            | Next (eff, f) ->
                match eff with
                | UpdateGlobalEff g -> state := g !state
                | TelegramEff teff -> 
                    let client = match (!state).client with RealClient f -> f() | NoneClient -> failwith "no client"
                    match teff with
                    | SendCodeRequest (phone, f) -> 
                        let! code = client.SendCodeRequestAsync phone
                        state := f code !state
                    | MakeAuthRequest (p, h, c) -> do! client.MakeAuthAsync(p, h, c)
                    | ResolveUsernamRequest (r, f) -> 
                        let! a = client.SendRequestAsync<TeleSharp.TL.Contacts.TLResolvedPeer>(r)
                        state := f a !state |> Result.unwrap
                    | GetHistoryRequest (p, l, f) ->
                        let! a = client.GetHistoryAsync(p, 0, 0, l)
                        state := f a !state
                return! invoke terminate f
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

    let reset = 
        request ^ fun r ctx -> async {
            let! result = Domain.resetClient' r ||> Interpretator.invoke
            return! Successful.OK result ctx }

    let setCode = 
        request ^ fun r ctx -> async {
            do! Domain.login r |> Interpretator.invoke ignore
            return! Successful.NO_CONTENT ctx }

    let simleRead (uri : string) =
        request ^ fun _ ctx -> async {
            let! result = Domain.getHistory' uri ||> Interpretator.invoke
            return! Successful.ok (Json.toJson result) ctx }

    let start pass =
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
