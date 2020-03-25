module Main exposing (main)

import Browser exposing (Document, document)
import Html as Html
import Login as Login
import Home as Home



main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = init }


type Msg
    = LoginMsg Login.Msg
    | HomeMsg Home.Msg


type Model
    = Login Login.Model
    | Home Home.Model


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
        updateHomePage = updatePage Home.update HomeMsg Home
        initHomePage = initPage Home.init Home HomeMsg
    in
    
    case ( msg, model ) of
        ( LoginMsg Login.Connected, Login (Login.PendingConnection userName) ) ->
            initHomePage userName

        ( LoginMsg loginMsg, Login login ) ->
            updateLoginPage loginMsg login

        ( HomeMsg homeMsg, Home home ) ->
            updateHomePage homeMsg home

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

        Home home ->
            Sub.map HomeMsg (Home.subscriptions home)


view : Model -> Document Msg
view model =
    let
        viewLoginPage = viewPage Login.view LoginMsg
        viewHomePage = viewPage Home.view HomeMsg
    in
        case model of
            Login login ->
                viewLoginPage login

            Home home ->
                viewHomePage home


viewPage : (pageModel -> Document pageMsg) -> (pageMsg -> Msg) -> pageModel -> Document Msg
viewPage viewer fromPageMsg model =
    let
        { title, body } = viewer model
    in
        { title = title, body = List.map (Html.map fromPageMsg) body}