module Login exposing (Model, Msg, init, subscriptions, update, view)

import Element as Element exposing (Attribute, Element)
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import Session as Session exposing (Session)
import Socket exposing (openConnection)
import Socket.ConnectionString as Conn


type Msg
    = UserNameEntered String
    | FormSubmit
    | LoginSuccessful
    | NoOp


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


view : Model -> Element Msg
view model =
    case model of
        InputtingUserName userName ->
            viewInput userName

        PendingConnection userName ->
            viewPending userName


viewInput : String -> Element Msg
viewInput currentUserName =
    Element.column []
        [ Input.text [ onEnter FormSubmit ]
            { text = currentUserName
            , label = Input.labelAbove [] (Element.text "Please enter your name: ")
            , onChange = UserNameEntered
            , placeholder = Nothing
            }
        , Input.button []
            { label = Element.text "Enter"
            , onPress = Just FormSubmit
            }
        ]


viewPending : String -> Element Msg
viewPending currentUserName =
    Element.column []
        [ Input.text []
            { text = currentUserName
            , label = Input.labelAbove [] (Element.text "Please enter your name: ")
            , onChange = always NoOp
            , placeholder = Nothing
            }
        , Input.button []
            { label = Element.text "Enter"
            , onPress = Nothing
            }
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
