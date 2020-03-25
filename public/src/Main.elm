port module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (Html, div, text)
import Login as Login


port userJoined : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = initModel }


type Msg
    = LoginMsg Login.Msg
    | UserJoined String


type Model
    = Login Login.Model
    | Home ConnectedData


type alias ConnectedData =
    { userName : String
    , message : String
    }


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

        ( UserJoined userName, Home data ) ->
            ( Home { data | message = userName ++ " joined" }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login login ->
            Sub.map LoginMsg (Login.subscriptions login)

        Home _ ->
            userJoined UserJoined


view : Model -> Document Msg
view model =
    case model of
        Login login ->
            let
                { title, body } = Login.view login
            in
                { title = title, body = List.map (Html.map LoginMsg) body}

        Home home ->
            { title = "Envolve - Home"
            , body = [ viewConnected home ]}


viewConnected : ConnectedData -> Html Msg
viewConnected data =
    div []
        [ div []
            [ text ("Hello " ++ data.userName) ]
        , div []
            [ text data.message ]
        ]
