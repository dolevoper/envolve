module Main exposing (main)

import Browser exposing (Document, document)
import Html as Html
import Login as Login
import Admin as Admin
import Guest as Guest



main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = init }


type Msg
    = LoginMsg Login.Msg
    | AdminMsg Admin.Msg
    | GuestMsg Guest.Msg


type Model
    = Login Login.Model
    | Admin Admin.Model
    | Guest Guest.Model


init : () -> ( Model, Cmd Msg )
init flags =
    initPage Login.init Login LoginMsg flags


initPage : (pageFlags -> ( pageModel, Cmd pageMsg )) -> (pageModel -> Model) -> (pageMsg -> Msg) -> pageFlags -> ( Model, Cmd Msg )
initPage initiator fromPageModel fromPageMsg flags =
    let
        ( pageModel, pageCmd ) = initiator flags
    in
        ( fromPageModel pageModel, Cmd.map fromPageMsg pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateLoginPage = updatePage Login.update LoginMsg Login
        updateAdminPage = updatePage Admin.update AdminMsg Admin
        updateGuestPage = updatePage Guest.update GuestMsg Guest
        initAdminPage = initPage Admin.init Admin AdminMsg
        initGuestPage = initPage Guest.init Guest GuestMsg
    in
    
    case ( msg, model ) of
        ( LoginMsg Login.Connected, Login (Login.PendingConnection userName) ) ->
            initGuestPage userName

        ( GuestMsg (Guest.Managing inviteLink), Guest { userName } ) ->
            initAdminPage { userName = userName, inviteLink = inviteLink }

        ( LoginMsg loginMsg, Login login ) ->
            updateLoginPage loginMsg login

        ( AdminMsg adminMsg, Admin admin ) ->
            updateAdminPage adminMsg admin

        ( GuestMsg guestMsg, Guest guest ) ->
            updateGuestPage guestMsg guest

        ( _, _ ) ->
            ( model, Cmd.none )


updatePage : (pageMsg -> pageModel -> ( pageModel, Cmd pageMsg )) -> (pageMsg -> Msg) -> (pageModel -> Model) -> pageMsg -> pageModel -> ( Model, Cmd Msg )
updatePage updater fromPageMsg fromPageModel msg model =
    let
        ( newModel, pageCmd ) = updater msg model
    in
        ( fromPageModel newModel, Cmd.map fromPageMsg pageCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login login ->
            Sub.map LoginMsg (Login.subscriptions login)

        Admin admin ->
            Sub.map AdminMsg (Admin.subscriptions admin)

        Guest guest ->
            Sub.map GuestMsg (Guest.subscriptions guest)


view : Model -> Document Msg
view model =
    let
        viewLoginPage = viewPage Login.view LoginMsg
        viewAdminPage = viewPage Admin.view AdminMsg
        viewGuestPage = viewPage Guest.view GuestMsg
    in
        case model of
            Login login ->
                viewLoginPage login

            Admin admin ->
                viewAdminPage admin
                
            Guest guest ->
                viewGuestPage guest


viewPage : (pageModel -> Document pageMsg) -> (pageMsg -> Msg) -> pageModel -> Document Msg
viewPage viewer fromPageMsg model =
    let
        { title, body } = viewer model
    in
        { title = title, body = List.map (Html.map fromPageMsg) body}