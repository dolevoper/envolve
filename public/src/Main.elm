port module Main exposing (main)

import Admin as Admin
import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Guest as Guest
import Html as Html
import Login as Login
import Url exposing (Protocol(..), Url)


port disconnected : (() -> msg) -> Sub msg


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
    | Disconnected
    | LoginMsg Login.Msg
    | AdminMsg Admin.Msg
    | GuestMsg Guest.Msg


type alias Model =
    { url : Url
    , page : Page
    }


type Page
    = Login Login.Model
    | Admin Admin.Model
    | Guest Guest.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url _ =
    toAppState url (initPage Login.init Login LoginMsg flags)


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
        schema =
            case model.url.protocol of
                Http ->
                    "http://"

                Https ->
                    "https://"

        port_ =
            Maybe.map (String.fromInt >> String.append ":") model.url.port_

        baseUrl =
            schema ++ model.url.host ++ Maybe.withDefault "" port_

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
        ( Disconnected, _ ) ->
            ( model, Nav.load baseUrl )

        ( LoginMsg Login.Connected, Login (Login.PendingConnection userName) ) ->
            toAppState model.url (initGuestPage userName)

        ( GuestMsg (Guest.Managing roomId), Guest { userName } ) ->
            toAppState model.url (initAdminPage { userName = userName, url = model.url, roomId = roomId })

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
            disconnected (always Disconnected)
    in
    case model.page of
        Login login ->
            Sub.map LoginMsg (Login.subscriptions login)

        Admin admin ->
            Sub.batch
                [ Sub.map AdminMsg (Admin.subscriptions admin)
                , disconnectedSubscription
                ]

        Guest guest ->
            Sub.batch
                [ Sub.map GuestMsg (Guest.subscriptions guest)
                , disconnectedSubscription
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
