port module Login exposing (FormState(..), Model, Msg(..), init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, for, id, type_, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Json
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (parse, string)
import Url.Builder as UrlBuilder exposing (crossOrigin)
import UrlUtils exposing (baseUrl)


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


type Msg
    = UserNameEntered String
    | FormSubmit
    | Connected


type alias Model =
    { baseUrl : String
    , roomId : Maybe String
    , userName : String
    , formState : FormState
    }


type FormState
    = InputtingUserName
    | PendingConnection


init : Url -> ( Model, Cmd Msg )
init url =
    ( { baseUrl = baseUrl url, roomId = parse string url, userName = "", formState = InputtingUserName }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.formState ) of
        ( UserNameEntered newUserName, InputtingUserName ) ->
            ( { model | userName = newUserName }, Cmd.none )

        ( FormSubmit, InputtingUserName ) ->
            ( { model | formState = PendingConnection }, connect (buildConnectionString model.baseUrl model.userName model.roomId) )

        ( Connected, PendingConnection ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    connected (always Connected)


view : Model -> Document Msg
view model =
    { title = "Envolve - Login"
    , body =
        case model.formState of
            InputtingUserName ->
                [ viewInput model.userName ]

            PendingConnection ->
                [ viewPending model.userName ]
    }


viewInput : String -> Html Msg
viewInput currentUserName =
    form [ onSubmit FormSubmit ]
        [ label [ for "name-input" ] [ text "Please enter your name: " ]
        , input [ onInput UserNameEntered, id "name-input", value currentUserName ] []
        , button [ type_ "submit" ] [ text "Enter" ]
        ]


viewPending : String -> Html Msg
viewPending currentUserName =
    div []
        [ label [ for "name-input" ] [ text "Please enter your name: " ]
        , input [ id "name-input", value currentUserName, disabled True ] []
        , button [ disabled True ] [ text "Enter" ]
        ]


buildConnectionString : String -> String -> Maybe String -> String
buildConnectionString baseUrl userName maybeRoomId = crossOrigin baseUrl []
    [ UrlBuilder.string "userName" userName
    , UrlBuilder.string "roomId" (Maybe.withDefault "" maybeRoomId)
    ]


onSubmit : Msg -> Html.Attribute Msg
onSubmit msg =
    preventDefaultOn "submit" (Json.succeed ( msg, True ))
