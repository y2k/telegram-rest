open System

[<AutoOpen>]
module Prelude =
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

module Domain = 
    open TeleSharp.TL
    open TeleSharp.TL.Contacts
    open TeleSharp.TL.Messages

    type Snapshot = { title : string; uri : Uri }

    let mkChatResolveRequest (uri : Uri) =
        let chatName = uri.Segments.[1]
        TLRequestResolveUsername(Username = chatName)

    let mkHistoryRequest (response : TLResolvedPeer) =
        if Seq.isEmpty response.Chats 
        then Error ""
        else
            let channel = response.Chats.[0] :?> TLChannel
            TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)
            |> Ok

    let parse (history : TLAbsMessages) chatName =
        (history :?> TLChannelMessages).Messages
        |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
        |> Seq.map ^ fun x ->
            { title = x.Message
              uri = Uri <| sprintf "https://t.me/%s/%i" chatName x.Id }
        |> Seq.rev
        |> Seq.toList

type Global =
    { client : unit -> TLSharp.Core.TelegramClient }
    with 
        static member instance = ref { client = fun _ -> failwith "Client not complete" }
        static member update (f : Global -> Global) =
            Global.instance := f !Global.instance
        static member askClient = (!Global.instance).client()

module TelegramConnector =
    open TLSharp.Core
    open TeleSharp.TL.Contacts

    let private log = printfn "LOG :: %s"

    let mkClient appId apiHash phone = async {
        let client = new TelegramClient(appId, apiHash)
        if client.IsUserAuthorized() then
            log "Telegram authorized"
            return client, ""
        else
            let! hash = client.SendCodeRequestAsync phone
            log ^ sprintf "Telegram required code (hash = %s)" hash
            return client, hash }

    let updateToken phone hash code = async {
        log ^ sprintf "Telegram setCode called, code = %s" code
        let! r = Global.askClient.MakeAuthAsync(phone, hash, code) |> Async.AwaitTask |> Async.Catch
        log ^ sprintf "Telegram code applied, result = %O" r }

    let getNodes (uri : Uri) = async {
        let! response =
            Domain.mkChatResolveRequest uri
            |> Global.askClient.SendRequestAsync<TLResolvedPeer>
        let r = Domain.mkHistoryRequest response |> Result.unwrap
        log ^ sprintf "Response = %O | id = %O" uri.Segments.[1] r.ChannelId
        let! history = Global.askClient.GetHistoryAsync(r, 0, 0, 50)
        return Domain.parse history }

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators
    open Suave.Form
    module P = TelegramConnector

    let simleRead (uri : string) ctx = async {
        let! result =
            Uri uri
            |> P.getNodes
            >>- (Json.toJson >> Successful.ok)
        return! result ctx }

    let reset = request ^ fun r ->
        let isInt : Validation<string> = (Int32.TryParse >> fst), (sprintf "must be at int"), ("", "")
        let formDesc : Form<{| appId : string; apiHash : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.appId @>), [ isInt ])
                    TextProp ((fun f -> <@ f.apiHash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        bindForm formDesc r
        |> Choice.map ^ fun x -> 
            fun ctx -> async {
                let! (client, hash) = P.mkClient (int x.appId) x.apiHash x.phone
                Global.update ^ fun g -> { g with client = always client }
                return! Successful.OK hash ctx }
        |> Choice.fold id RequestErrors.BAD_REQUEST

    let setCode = request ^ fun r ->
        let formDesc : Form<{| hash : string; code : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.hash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.code @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 32 ]) ], [])
        bindForm formDesc r 
        |> Choice.map ^ fun x -> 
            fun ctx -> async {
                do! P.updateToken x.phone x.hash x.code
                return! Successful.NO_CONTENT ctx }
        |> Choice.fold id RequestErrors.BAD_REQUEST

    let getStatus ctx = async {
        let isConnected = Global.askClient.IsConnected
        let isAuthorized = Global.askClient.IsUserAuthorized()
        return!
            sprintf "Status: isConnected = %b; isAuthorized = %b" isConnected isAuthorized
            |> flip Successful.OK ctx  }

    let start pass =
        choose [
            POST >=> pathScan "/simple-read/%s" simleRead
            Authentication.authenticateBasic
                (fun (_, p) -> p = pass)
                ^ choose [
                    POST >=> path "/reset" >=> reset
                    POST >=> path "/set-code" >=> setCode
                    GET >=> path "/status" >=> getStatus ] ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    Server.start args.[0] |> Async.RunSynchronously
    0
