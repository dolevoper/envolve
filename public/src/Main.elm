module Main exposing (main)

import Browser exposing (Document, document)
import Html as Html
import Login as Login
import Home as Home



main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = initModel }


type Msg
    = LoginMsg Login.Msg
    | HomeMsg Home.Msg


type Model
    = Login Login.Model
    | Home Home.Model


initModel : () -> ( Model, Cmd Msg )
initModel flags =
    let
        ( loginModel, loginCmd ) = Login.init flags
    in
        ( Login loginModel, Cmd.map LoginMsg loginCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoginMsg Login.Connected, Login (Login.PendingConnection userName) ) ->
            ( Home { userName = userName, message = "" }, Cmd.none )

        ( LoginMsg loginMsg, Login login ) ->
            let
                ( newLogin, loginCmd ) = Login.update loginMsg login
            in
                ( Login newLogin, Cmd.map LoginMsg loginCmd )

        ( HomeMsg homeMsg, Home home ) ->
            let
                ( newHome, homeCmd ) = Home.update homeMsg home
            in
                ( Home newHome, Cmd.map HomeMsg homeCmd )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login login ->
            Sub.map LoginMsg (Login.subscriptions login)

        Home home ->
            Sub.map HomeMsg (Home.subscriptions home)


view : Model -> Document Msg
view model =
    case model of
        Login login ->
            let
                { title, body } = Login.view login
            in
                { title = title, body = List.map (Html.map LoginMsg) body}

        Home home ->
            let
                { title, body } = Home.view home
            in
                { title = title, body = List.map (Html.map HomeMsg) body}
