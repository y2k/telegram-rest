open System

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (>>-) a f = async {
        let! x = a
        return f x }

module TelegramConnector =
    open TLSharp.Core
    open TLSharp.Core.Network
    open TeleSharp.TL
    open TeleSharp.TL.Contacts
    open TeleSharp.TL.Messages

    module L =
        let log = printfn "LOG :: %s"

    type Snapshot =
        { title : string
          uri : Uri }

    let mkClient appId apiHash phone = async {
        let client = new TelegramClient(appId, apiHash)
        if client.IsUserAuthorized() then
            L.log "Telegram authorized"
            return client, ""
        else
            let! hash = client.SendCodeRequestAsync phone |> Async.AwaitTask
            L.log ^ sprintf "Telegram required code (hash = %s)" hash
            return client, hash }

    let updateToken (client : TelegramClient) phone hash code = async {
        L.log ^ sprintf "Telegram setCode called, code = %s" code
        let! r = client.MakeAuthAsync(phone, hash, code) |> Async.AwaitTask |> Async.Catch
        L.log ^ sprintf "Telegram code applied, result = %O" r }

    let getNodes (client : TelegramClient) (uri : Uri) = async {
        let chatName = uri.Segments.[1]
        let r = TLRequestResolveUsername()
        r.Username <- chatName
        let! response = client.SendRequestAsync r |> Async.AwaitTask
        let channel = (response :> TLResolvedPeer).Chats.[0] :?> TLChannel
        L.log ^ sprintf "Response = %O | id = %O" channel.Username channel.Id
        let i = TLInputPeerChannel()
        i.ChannelId <- channel.Id
        i.AccessHash <- channel.AccessHash.Value
        let! history = client.GetHistoryAsync(i, 0, 0, 50) |> Async.AwaitTask
        return
            (history :?> TLChannelMessages).Messages
            |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
            |> Seq.map ^
                fun x ->
                    { title = x.Message
                      uri = Uri <| sprintf "https://t.me/%s/%i" chatName x.Id }
            |> Seq.rev
            |> Seq.toList }

type Global =
    { client : TLSharp.Core.TelegramClient }
    with 
        static member instance () : Global = failwith "???"
        static member set (_ : Global) = () // FIXME:

module Server =
    open Suave
    open Suave.Filters
    open Suave.Operators
    module P = TelegramConnector

    let simleRead (uri : string) ctx = async {
        let! r =
            Uri uri
            |> P.getNodes (Global.instance().client)
            >>- (Json.toJson >> Successful.ok)
        return! r ctx }

    let reset = request ^ fun r ->
        match r.formData "appId", r.formData "apiHash", r.formData "phone" with
        | Choice1Of2 appId, Choice1Of2 apiHash, Choice1Of2 phone ->
            fun ctx -> async {
                let! (client, hash) = P.mkClient (int appId) apiHash phone
                Global.set { client = client }
                return! Successful.OK hash ctx }
        | _ -> RequestErrors.BAD_REQUEST ""

    let setCode = request ^ fun r ->
        match r.formData "phone", r.formData "hash", r.formData "code" with
        | Choice1Of2 phone, Choice1Of2 hash, Choice1Of2 code ->
            fun ctx -> async {
                do! P.updateToken (Global.instance().client) phone hash code
                return! Successful.NO_CONTENT ctx }
        | _ -> RequestErrors.BAD_REQUEST ""

    let start pass =
        choose [
            GET >=> pathScan "/simple-read/%s" simleRead
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
