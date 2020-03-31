module Login exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, for, id, type_, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Json
import Session as Session exposing (Session)
import Socket exposing (openConnection)
import Socket.ConnectionString as Conn


type Msg
    = UserNameEntered String
    | FormSubmit
    | LoginSuccessful


type Model
    = InputtingUserName String
    | PendingConnection String


init : ( Model, Cmd Msg )
init =
    ( InputtingUserName "", Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update session msg model =
    case ( msg, model ) of
        ( UserNameEntered newUserName, InputtingUserName _ ) ->
            ( InputtingUserName newUserName, Cmd.none, Nothing )

        ( FormSubmit, InputtingUserName userName ) ->
            let
                connectionString =
                    Conn.fromUrl (Session.url session) userName
            in
            ( PendingConnection userName, openConnection connectionString, Nothing )

        ( LoginSuccessful, PendingConnection userName ) ->
            ( model, Cmd.none, Just userName )

        ( _, _ ) ->
            ( model, Cmd.none, Nothing )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Socket.connected (always LoginSuccessful)


view : Model -> Html Msg
view model =
    case model of
        InputtingUserName userName ->
            viewInput userName

        PendingConnection userName ->
            viewPending userName


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
