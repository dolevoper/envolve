port module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (button, div, form, input, label, text)
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


type alias Model =
    { nameInputText : String
    , currentName : String
    , message : String
    }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( { nameInputText = "", currentName = "", message = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName str ->
            ( { model | nameInputText = str }, Cmd.none )

        SubmitName ->
            ( { model | currentName = model.nameInputText, nameInputText = "" }, connect model.nameInputText )

        UserJoined userName ->
            ( { model | message = userName ++ " joined" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    userJoined UserJoined


view : Model -> Document Msg
view model =
    { title = "Envolve"
    , body =
        [ div []
            [ form [ onSubmit SubmitName ]
                [ label [ for "name-input" ] [ text "Please enter your name: " ]
                , input [ onInput UpdateName, id "name-input", value model.nameInputText ] []
                , button [ type_ "submit" ] [ text "Enter" ]
                , text model.currentName
                ]
            , div [] [ text model.message ]
            ]
        ]
    }


onSubmit : Msg -> Html.Attribute Msg
onSubmit msg =
    preventDefaultOn "submit" (Json.succeed ( msg, True ))
