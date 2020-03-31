module Main exposing (main)

import Admin
import Browser as Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Guest
import Html as Html exposing (Html)
import Html.Attributes as Attrs
import Login
import RoomId exposing (RoomId)
import Route
import Session as Session exposing (Session)
import Socket
import Socket.Events as Events
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- MODEL --


type Model
    = CreatingRoom Session Login.Model
    | PendingRoomId Session String
    | ManagingRoom Session Admin.Model
    | JoiningRoom Session RoomId Login.Model
    | ViewingRoom Session Guest.Model
    | Error Session String


session : Model -> Session
session model =
    case model of
        CreatingRoom s _ ->
            s

        PendingRoomId s _ ->
            s

        ManagingRoom s _ ->
            s

        JoiningRoom s _ _ ->
            s

        ViewingRoom s _ ->
            s

        Error s _ ->
            s


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    appState (Session.newSession url key) url



-- UPDATE --


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | LoginMsg Login.Msg
    | GotRoomId RoomId
    | GotError String
    | AdminMsg Admin.Msg
    | GuestMsg Guest.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            appState (Session.updateUrl url <| session model) url

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Session.pushUrl (session model) (Url.toString url) )

        ( LoginMsg loginMsg, CreatingRoom s login ) ->
            let
                ( updatedLogin, cmd, loginResult ) =
                    Login.update s loginMsg login
            in
            case loginResult of
                Nothing ->
                    ( CreatingRoom s updatedLogin, Cmd.map LoginMsg cmd )

                Just userName ->
                    ( PendingRoomId s userName, Cmd.none )

        ( LoginMsg loginMsg, JoiningRoom s roomId login ) ->
            let
                ( updatedLogin, cmd, loginResult ) =
                    Login.update s loginMsg login
            in
            case loginResult of
                Nothing ->
                    ( JoiningRoom s roomId updatedLogin, Cmd.map LoginMsg cmd )

                Just userName ->
                    let
                        updatedSession =
                            Session.login roomId userName s
                    in
                    case updatedSession of
                        Err message ->
                            ( Error s message, Cmd.none )

                        Ok newSession ->
                            ( JoiningRoom newSession roomId updatedLogin, Session.pushUrl s (Route.toString <| Route.ViewRoom roomId) )

        ( GotRoomId roomId, PendingRoomId s userName ) ->
            let
                updatedSession =
                    Session.login roomId userName s
            in
            case updatedSession of
                Err message ->
                    ( Error s message, Cmd.none )

                Ok loggedInSession ->
                    ( PendingRoomId loggedInSession userName, Session.pushUrl s (Route.toString <| Route.ManageRoom roomId) )

        ( AdminMsg adminMsg, ManagingRoom s admin ) ->
            Tuple.mapBoth (ManagingRoom s) (Cmd.map AdminMsg) (Admin.update adminMsg admin)

        ( GuestMsg guestMsg, ViewingRoom s guest ) ->
            Tuple.mapBoth (ViewingRoom s) (Cmd.map GuestMsg) (Guest.update s guestMsg guest)

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Document Msg
view model =
    { title = "Envolve"
    , body = [ viewPage model ]
    }


viewPage : Model -> Html Msg
viewPage model =
    case model of
        CreatingRoom _ login ->
            Html.map LoginMsg (Login.view login)

        PendingRoomId _ _ ->
            Html.div [] [ Html.text "Connecting you to your room..." ]

        JoiningRoom _ _ login ->
            Html.map LoginMsg (Login.view login)

        ManagingRoom s admin ->
            Html.map AdminMsg (Admin.view s admin)

        ViewingRoom s guest ->
            Html.map GuestMsg (Guest.view s guest)

        Error _ msg ->
            Html.div []
                [ Html.text msg
                , Html.br [] []
                , Html.a [ Attrs.href "/" ] [ Html.text "Create a new room" ]
                ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        CreatingRoom _ login ->
            Sub.map LoginMsg (Login.subscriptions login)

        PendingRoomId _ _ ->
            Socket.listen NoOp (Events.managing.inBound GotError GotRoomId)

        ManagingRoom _ admin ->
            Sub.map AdminMsg (Admin.subscriptions admin)

        JoiningRoom _ _ login ->
            Sub.map LoginMsg (Login.subscriptions login)

        ViewingRoom _ guest ->
            Sub.map GuestMsg (Guest.subscriptions guest)

        _ ->
            Sub.none



-- PRIVATE HELPERS --


appState : Session -> Url -> ( Model, Cmd Msg )
appState s url =
    let
        route =
            Route.fromUrl url
    in
    case ( route, Session.isLoggedIn s ) of
        ( Route.CreateRoom, _ ) ->
            Tuple.mapBoth (CreatingRoom s) (Cmd.map LoginMsg) Login.init

        ( Route.JoinRoom roomId, _ ) ->
            Tuple.mapBoth (JoiningRoom s roomId) (Cmd.map LoginMsg) Login.init

        ( Route.ManageRoom _, True ) ->
            Tuple.mapBoth (ManagingRoom s) (Cmd.map AdminMsg) Admin.init

        ( Route.ViewRoom _, True ) ->
            Tuple.mapBoth (ViewingRoom s) (Cmd.map GuestMsg) Guest.init

        ( _, False ) ->
            ( Error s "you're not logged in!", Cmd.none )

        ( Route.NotFound, _ ) ->
            ( Error s "not found", Cmd.none )
