open System

[<AutoOpen>]
module Prelude =
    let inline flip f a b = f b a
    let inline always x _ = x
    let inline (^) f x = f x
    let inline (>>-) a f = async {
        let! x = a
        return f x }

module Domain = 
    open TeleSharp.TL
    open TeleSharp.TL.Messages

    type Snapshot =
        { title : string
          uri : Uri }

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
        static member instance = ref { client = fun _ -> failwith "???" }
        static member update (f : Global -> Global) =
            Global.instance := f !Global.instance
        static member client' = (!Global.instance).client()

module TelegramConnector =
    open TLSharp.Core
    open TeleSharp.TL
    open TeleSharp.TL.Contacts

    let private log = printfn "LOG :: %s"

    let mkClient appId apiHash phone = async {
        let client = new TelegramClient(appId, apiHash)
        if client.IsUserAuthorized() then
            log "Telegram authorized"
            return client, ""
        else
            let! hash = client.SendCodeRequestAsync phone |> Async.AwaitTask
            log ^ sprintf "Telegram required code (hash = %s)" hash
            return client, hash }

    let updateToken phone hash code = async {
        log ^ sprintf "Telegram setCode called, code = %s" code
        let! r = Global.client'.MakeAuthAsync(phone, hash, code) |> Async.AwaitTask |> Async.Catch
        log ^ sprintf "Telegram code applied, result = %O" r }

    let getNodes (uri : Uri) = async {
        let chatName = uri.Segments.[1]
        let! response =
            TLRequestResolveUsername(Username = chatName)
            |> Global.client'.SendRequestAsync
            |> Async.AwaitTask
        let channel = (response :> TLResolvedPeer).Chats.[0] :?> TLChannel
        log ^ sprintf "Response = %O | id = %O" channel.Username channel.Id
        let i = TLInputPeerChannel(ChannelId = channel.Id, AccessHash = channel.AccessHash.Value)
        let! history = Global.client'.GetHistoryAsync(i, 0, 0, 50) |> Async.AwaitTask
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
        let formDesc : Form<{| appId : string; apiHash : string; phone : string |}> =
            Form ([ TextProp ((fun f -> <@ f.appId @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.apiHash @>), [ maxLength 256 ])
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 256 ]) ], [])
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
                    TextProp ((fun f -> <@ f.phone @>), [ maxLength 256 ]) ], [])
        bindForm formDesc r 
        |> Choice.map ^ fun x -> 
            fun ctx -> async {
                do! P.updateToken x.phone x.hash x.code
                return! Successful.NO_CONTENT ctx }
        |> Choice.fold id RequestErrors.BAD_REQUEST

    let start pass =
        choose [
            POST >=> pathScan "/simple-read/%s" simleRead
            Authentication.authenticateBasic
                (fun (_, p) -> p = pass)
                ^ choose [
                    POST >=> path "/reset" >=> reset
                    POST >=> path "/set-code" >=> setCode
                    GET >=> path "/status" >=> Successful.OK "FIXME" ] ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
        |> snd

[<EntryPoint>]
let main args =
    Server.start args.[0] |> Async.RunSynchronously
    0
