module Main exposing (main)

import Admin as Admin
import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Guest as Guest
import Html as Html
import Login as Login
import Url exposing (Protocol(..), Url)
import Socket as Socket
import UrlUtils exposing (baseUrl)
import Dict as Dict
import Json.Decode as Decode


main : Program () Model Msg
main =
    application
        { view = view
        , subscriptions = subscriptions
        , update = update
        , init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Connected
    | Managing (Result String String)
    | Disconnected
    | LoginMsg Login.Msg
    | AdminMsg Admin.Msg
    | GuestMsg Guest.Msg
    | NoOp


type alias Model =
    { url : Url
    , page : Page
    }


type Page
    = Login Login.Model
    | Admin Admin.Model
    | Guest Guest.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    toAppState url (initPage Login.init Login LoginMsg url)


initPage : (pageFlags -> ( pageModel, Cmd pageMsg )) -> (pageModel -> Page) -> (pageMsg -> Msg) -> pageFlags -> ( Page, Cmd Msg )
initPage initiator fromPageModel fromPageMsg flags =
    let
        ( pageModel, pageCmd ) =
            initiator flags
    in
    ( fromPageModel pageModel, Cmd.map fromPageMsg pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateLoginPage =
            updatePage Login.update LoginMsg Login

        updateAdminPage =
            updatePage Admin.update AdminMsg Admin

        updateGuestPage =
            updatePage Guest.update GuestMsg Guest

        initAdminPage =
            initPage Admin.init Admin AdminMsg

        initGuestPage =
            initPage Guest.init Guest GuestMsg
    in
    case ( msg, model.page ) of
        ( Connected, Login { userName } ) ->
            toAppState model.url (initGuestPage userName)

        ( Managing (Ok roomId), Guest { userName } ) ->
            toAppState model.url (initAdminPage { userName = userName, url = model.url, roomId = roomId })

        ( Disconnected, _ ) ->
            ( model, Nav.load (baseUrl model.url) )

        ( LoginMsg loginMsg, Login login ) ->
            toAppState model.url (updateLoginPage loginMsg login)

        ( AdminMsg adminMsg, Admin admin ) ->
            toAppState model.url (updateAdminPage adminMsg admin)

        ( GuestMsg guestMsg, Guest guest ) ->
            toAppState model.url (updateGuestPage guestMsg guest)

        ( _, _ ) ->
            ( model, Cmd.none )


updatePage : (pageMsg -> pageModel -> ( pageModel, Cmd pageMsg )) -> (pageMsg -> Msg) -> (pageModel -> Page) -> pageMsg -> pageModel -> ( Page, Cmd Msg )
updatePage updater fromPageMsg fromPageModel msg model =
    let
        ( newModel, pageCmd ) =
            updater msg model
    in
    ( fromPageModel newModel, Cmd.map fromPageMsg pageCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        disconnectedSubscription =
            Socket.disconnected (always Disconnected)
        decodeString = Decode.decodeValue Decode.string >> Result.mapError Decode.errorToString
    in
    case model.page of
        Login login ->
            Sub.batch
                [ Sub.map LoginMsg (Login.subscriptions login)
                , Socket.connected (always Connected)
                ]

        Admin admin ->
            Sub.batch
                [ Sub.map AdminMsg (Admin.subscriptions admin)
                , disconnectedSubscription
                ]

        Guest guest ->
            Sub.batch
                [ Sub.map GuestMsg (Guest.subscriptions guest)
                , disconnectedSubscription
                , Socket.on NoOp NoOp
                    ( Dict.fromList
                        [ ( "managing", Socket.EventWithPayload (decodeString >> Managing) ) ]
                    )
                ]


view : Model -> Document Msg
view model =
    let
        viewLoginPage =
            viewPage Login.view LoginMsg

        viewAdminPage =
            viewPage Admin.view AdminMsg

        viewGuestPage =
            viewPage Guest.view GuestMsg
    in
    case model.page of
        Login login ->
            viewLoginPage login

        Admin admin ->
            viewAdminPage admin

        Guest guest ->
            viewGuestPage guest


viewPage : (pageModel -> Document pageMsg) -> (pageMsg -> Msg) -> pageModel -> Document Msg
viewPage viewer fromPageMsg model =
    let
        { title, body } =
            viewer model
    in
    { title = title, body = List.map (Html.map fromPageMsg) body }


toAppState : Url -> ( Page, Cmd Msg ) -> ( Model, Cmd Msg )
toAppState url pageState =
    Tuple.mapFirst (Model url) pageState
