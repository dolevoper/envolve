port module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Json


port connect : String -> Cmd msg


port userJoined : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    document { view = view, subscriptions = subscriptions, update = update, init = initModel }


type Msg
    = UpdateName String
    | SubmitName
    | UserJoined String


type Model
    = Login LoginData
    | Connected ConnectedData


type alias LoginData =
    String


type alias ConnectedData =
    { userName : String
    , message : String
    }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( Login "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateName newUserName, Login _ ) ->
            ( Login newUserName, Cmd.none )

        ( SubmitName, Login currentUserName ) ->
            ( Connected { userName = currentUserName, message = "" }, connect currentUserName )

        ( UserJoined userName, Connected data ) ->
            ( Connected { data | message = userName ++ " joined" }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    userJoined UserJoined


view : Model -> Document Msg
view model =
    { title = "Envolve"
    , body =
        case model of
            Login currentUserName ->
                [ viewLogin currentUserName ]

            Connected data ->
                [ viewConnected data ]
    }


viewLogin : LoginData -> Html Msg
viewLogin currentUserName =
    form [ onSubmit SubmitName ]
        [ label [ for "name-input" ] [ text "Please enter your name: " ]
        , input [ onInput UpdateName, id "name-input", value currentUserName ] []
        , button [ type_ "submit" ] [ text "Enter" ]
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
