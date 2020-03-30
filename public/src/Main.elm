module Main exposing (main)

import Admin as Admin
import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Guest as Guest
import Html as Html
import Login as Login
import Socket as Socket
import Socket.Events exposing (managing)
import Url exposing (Url)
import UrlUtils exposing (baseUrl)


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
    | Managing (Socket.IncomingMessage String)
    | Disconnected
    | LoginMsg Login.Msg
    | AdminMsg Admin.Msg
    | GuestMsg Guest.Msg
    | Error String
    | NoOp


type Model
    = AppError String
    | AppOk Url Page


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
    case model of
        AppError _ ->
            ( model, Cmd.none )

        AppOk url page ->
            let
                updateAdminPage =
                    updatePage Admin.update AdminMsg Admin

                updateGuestPage =
                    updatePage Guest.update GuestMsg Guest

                initAdminPage =
                    initPage Admin.init Admin AdminMsg

                initGuestPage =
                    initPage Guest.init Guest GuestMsg
            in
            case ( msg, page ) of
                ( Managing (Ok roomId), Guest { userName } ) ->
                    toAppState url (initAdminPage { userName = userName, url = url, roomId = roomId })

                ( Disconnected, _ ) ->
                    ( model, Nav.load (baseUrl url) )

                ( Error err, _ ) ->
                    ( AppError err, Cmd.none )

                ( LoginMsg loginMsg, Login login ) ->
                    let
                        ( newLoginModel, cmd, loginSuccessful ) =
                            Login.update loginMsg login
                    in
                    if loginSuccessful then
                        toAppState url (initGuestPage newLoginModel.userName)

                    else
                        toAppState url ( Login newLoginModel, Cmd.map LoginMsg cmd )

                ( AdminMsg adminMsg, Admin admin ) ->
                    toAppState url (updateAdminPage adminMsg admin)

                ( GuestMsg guestMsg, Guest guest ) ->
                    toAppState url (updateGuestPage guestMsg guest)

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
    case model of
        AppError _ ->
            Sub.none

        AppOk _ page ->
            let
                disconnectedSubscription =
                    Socket.disconnected (always Disconnected)

                errorSubscription =
                    Socket.socketError Error
            in
            case page of
                Login login ->
                    Sub.batch
                        [ Sub.map LoginMsg (Login.subscriptions login)
                        , errorSubscription
                        ]

                Admin admin ->
                    Sub.batch
                        [ Sub.map AdminMsg (Admin.subscriptions admin)
                        , disconnectedSubscription
                        , errorSubscription
                        ]

                Guest guest ->
                    Sub.batch
                        [ Sub.map GuestMsg (Guest.subscriptions guest)
                        , Socket.listen NoOp (managing.inBound Managing)
                        , disconnectedSubscription
                        , errorSubscription
                        ]


view : Model -> Document Msg
view model =
    case model of
        AppError err ->
            { title = "Envolve", body = [ Html.div [] [ Html.text err ] ] }

        AppOk _ page ->
            let
                viewLoginPage =
                    viewPage Login.view LoginMsg

                viewAdminPage =
                    viewPage Admin.view AdminMsg

                viewGuestPage =
                    viewPage Guest.view GuestMsg
            in
            case page of
                Login login ->
                    viewLoginPage login

                Admin admin ->
                    viewAdminPage admin

                Guest guest ->
                    viewGuestPage guest


viewPage : (pageModel -> Html.Html pageMsg) -> (pageMsg -> Msg) -> pageModel -> Document Msg
viewPage viewer fromPageMsg model =
    { title = "Envolve", body = [ Html.map fromPageMsg (viewer model) ] }


toAppState : Url -> ( Page, Cmd Msg ) -> ( Model, Cmd Msg )
toAppState url pageState =
    Tuple.mapFirst (AppOk url) pageState
