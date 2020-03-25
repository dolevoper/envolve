port module Login exposing (Msg(..), Model(..), init, update, view, subscriptions)

import Browser exposing (Document)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, for, id, type_, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Json


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


type Msg
    = UserNameEntered String
    | FormSubmit
    | Connected


type Model
    = InputtingUserName String
    | PendingConnection String


init : () -> ( Model, Cmd Msg )
init _ =
    ( InputtingUserName "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UserNameEntered newUserName, InputtingUserName _ ) ->
            ( InputtingUserName newUserName, Cmd.none )

        ( FormSubmit, InputtingUserName currentUserName ) ->
            ( PendingConnection currentUserName, connect currentUserName )

        ( Connected, PendingConnection _ ) ->
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
        case model of
            InputtingUserName currentUserName ->
                [ viewInput currentUserName ]

            PendingConnection currentUserName ->
                [ viewPending currentUserName ]
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


onSubmit : Msg -> Html.Attribute Msg
onSubmit msg =
    preventDefaultOn "submit" (Json.succeed ( msg, True ))
