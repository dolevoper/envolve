module Present exposing (Model, Msg, init, subscriptions, update, view)

import Element as El exposing (Element)
import Session as Session exposing (Session)
import Socket
import Socket.ConnectionString as Conn


type Model
    = PendingDisconnection
    | PendingConnection
    | Presenting
    | PresentationEnded


init : Session -> ( Model, Cmd msg )
init session =
    if Session.isLoggedIn session then
        ( PendingDisconnection, Socket.disconnect () )

    else
        ( PendingConnection, openConnection session )


type Msg
    = Disconnected
    | Connected


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( msg, model ) of
        ( Disconnected, PendingDisconnection ) ->
            ( PendingConnection, openConnection session )

        ( Connected, PendingConnection ) ->
            ( Presenting, Cmd.none )

        ( Disconnected, Presenting ) ->
            ( PresentationEnded, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Element msg
view model =
    case model of
        PendingDisconnection ->
            El.text "pending disconnection..."

        PendingConnection ->
            El.text "pending connection..."

        Presenting ->
            El.text "presenting!"

        PresentationEnded ->
            El.text "presentation ended."


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PendingDisconnection ->
            Socket.disconnected <| always Disconnected

        PendingConnection ->
            Socket.connected <| always Connected

        Presenting ->
            Socket.disconnected <| always Disconnected

        PresentationEnded ->
            Sub.none


openConnection : Session -> Cmd msg
openConnection session =
    Socket.openConnection <| Conn.fromUrl (Session.url session) Nothing
