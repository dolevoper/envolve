module Main exposing (main)

import Admin
import Browser as Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Color.OneDark as Colors
import Element as El exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Guest
import Http
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
    | CheckingRoomExists Session RoomId
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

        CheckingRoomExists s _ ->
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
    | RoomExistsResult (Result Http.Error ())
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

        ( RoomExistsResult result, CheckingRoomExists s roomId ) ->
            case result of
                Err (Http.BadStatus 404) ->
                    ( Error s ("room " ++ RoomId.toString roomId ++ " does not exist"), Cmd.none )

                Err _ ->
                    ( Error s "oops something went wrong!", Cmd.none )

                Ok _ ->
                    Tuple.mapBoth (JoiningRoom s roomId) (Cmd.map LoginMsg) Login.init

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

        ( GotError message, _ ) ->
            ( Error (session model) message, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Document Msg
view model =
    { title = "Envolve"
    , body =
        [ El.layoutWith
            { options =
                [ El.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Font.family [ Font.typeface "Roboto" ]
            , Font.color Colors.black
            ]
          <|
            El.column
                [ El.height El.fill
                , El.width El.fill
                , El.spacing 20
                ]
                [ viewHeader
                , viewPage model
                ]
        ]
    }


viewHeader : Element msg
viewHeader =
    El.row
        [ Bg.color Colors.black
        , Font.color Colors.white
        , El.width El.fill
        , El.padding 20
        , Border.shadow
            { blur = 10
            , color = Colors.black
            , offset = ( 0, 0 )
            , size = 1
            }
        ]
        [ El.text "Envolve" ]


viewPage : Model -> Element Msg
viewPage model =
    case model of
        CreatingRoom _ login ->
            El.map LoginMsg (Login.view login)

        PendingRoomId _ _ ->
            El.text "Connecting you to your room..."

        CheckingRoomExists _ roomId ->
            El.text ("Looking for room " ++ RoomId.toString roomId)

        JoiningRoom _ _ login ->
            El.map LoginMsg (Login.view login)

        ManagingRoom s admin ->
            El.map AdminMsg (Admin.view s admin)

        ViewingRoom s guest ->
            El.map GuestMsg (Guest.view s guest)

        Error _ msg ->
            El.column []
                [ El.text msg
                , El.link []
                    { label = El.text "Create a new room"
                    , url = "/"
                    }
                ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        disconnectedSub =
            Socket.disconnected (always (GotError "you got disconnected from the server"))
    in
    case model of
        CreatingRoom _ login ->
            Sub.map LoginMsg (Login.subscriptions login)

        PendingRoomId _ _ ->
            Sub.batch
                [ Socket.listen NoOp (Events.managing.inBound GotError GotRoomId)
                , disconnectedSub
                ]

        ManagingRoom _ admin ->
            Sub.batch
                [ Sub.map AdminMsg (Admin.subscriptions admin)
                , disconnectedSub
                ]

        JoiningRoom _ _ login ->
            Sub.map LoginMsg (Login.subscriptions login)

        ViewingRoom _ guest ->
            Sub.batch
                [ Sub.map GuestMsg (Guest.subscriptions guest)
                , disconnectedSub
                ]

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
            ( CheckingRoomExists s roomId
            , Http.get
                { url = "/api/rooms/" ++ RoomId.toString roomId
                , expect = Http.expectWhatever RoomExistsResult
                }
            )

        ( Route.ManageRoom _, True ) ->
            Tuple.mapBoth (ManagingRoom s) (Cmd.map AdminMsg) Admin.init

        ( Route.ViewRoom _, True ) ->
            Tuple.mapBoth (ViewingRoom s) (Cmd.map GuestMsg) Guest.init

        ( _, False ) ->
            ( Error s "you're not logged in!", Cmd.none )

        ( Route.NotFound, _ ) ->
            ( Error s "not found", Cmd.none )
