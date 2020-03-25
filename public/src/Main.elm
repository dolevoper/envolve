port module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (for, id, type_, value, disabled)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Json


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port userJoined : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = initModel }


type Msg
    = UpdateName String
    | SubmitName
    | Connected
    | UserJoined String


type Model
    = Login LoginData
    | Home ConnectedData


type LoginData = Input String | Pending String


type alias ConnectedData =
    { userName : String
    , message : String
    }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( Login (Input ""), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateName newUserName, Login (Input _) ) ->
            ( Login (Input newUserName), Cmd.none )

        ( SubmitName, Login (Input currentUserName) ) ->
            ( Login (Pending currentUserName), connect currentUserName )

        ( Connected, Login (Pending currentUserName) ) ->
            ( Home { userName = currentUserName, message = "" }, connect currentUserName )

        ( UserJoined userName, Home data ) ->
            ( Home { data | message = userName ++ " joined" }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login _ ->
            connected (always Connected)

        Home _ ->
            userJoined UserJoined


view : Model -> Document Msg
view model =
    { title = "Envolve"
    , body =
        case model of
            Login data ->
                [ viewLogin data ]

            Home data ->
                [ viewConnected data ]
    }


viewLogin : LoginData -> Html Msg
viewLogin data =
    case data of
        Input currentUserName ->
            form [ onSubmit SubmitName ]
                [ label [ for "name-input" ] [ text "Please enter your name: " ]
                , input [ onInput UpdateName, id "name-input", value currentUserName ] []
                , button [ type_ "submit" ] [ text "Enter" ]
                ]

        Pending currentUserName ->
            div []
                [ label [ for "name-input" ] [ text "Please enter your name: " ]
                , input [ id "name-input", value currentUserName, disabled True ] []
                , button [ disabled True ] [ text "Enter" ]
                ]


viewConnected : ConnectedData -> Html Msg
viewConnected data =
    div []
        [ div []
            [ text ("Hello " ++ data.userName) ]
        , div []
            [ text data.message ]
        ]


onSubmit : Msg -> Html.Attribute Msg
onSubmit msg =
    preventDefaultOn "submit" (Json.succeed ( msg, True ))
