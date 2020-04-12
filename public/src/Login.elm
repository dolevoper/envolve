module Login exposing (Model, Msg, init, subscriptions, update, view)

import Color.OneDark as Colors
import Element as El exposing (Attribute, Element)
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import PrimaryButton exposing (primaryButton)
import Session as Session exposing (Session)
import Socket exposing (openConnection)
import Socket.ConnectionString as Conn


type Msg
    = UserNameEntered String
    | FormSubmit
    | LoginSuccessful
    | NoOp


type Model
    = Pristine
    | SomeUserName String
    | Empty
    | PendingConnection String


init : ( Model, Cmd Msg )
init =
    ( Pristine, Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update session msg model =
    case ( msg, model ) of
        ( UserNameEntered newUserName, Pristine ) ->
            ( SomeUserName newUserName, Cmd.none, Nothing )

        ( FormSubmit, Pristine ) ->
            ( Empty, Cmd.none, Nothing )

        ( UserNameEntered newUserName, SomeUserName _ ) ->
            if String.isEmpty newUserName then
                ( Empty, Cmd.none, Nothing )

            else
                ( SomeUserName newUserName, Cmd.none, Nothing )

        ( FormSubmit, SomeUserName userName ) ->
            let
                connectionString =
                    Conn.fromUrl (Session.url session) (Just userName)
            in
            ( PendingConnection userName, openConnection connectionString, Nothing )

        ( UserNameEntered newUserName, Empty ) ->
            ( SomeUserName newUserName, Cmd.none, Nothing )

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
        Pristine ->
            viewInput "" ""

        SomeUserName userName ->
            viewInput userName ""

        Empty ->
            viewInput "" "User name cannot be empty"

        PendingConnection userName ->
            viewPending userName


viewInput : String -> String -> Element Msg
viewInput currentUserName message =
    El.column
        [ El.centerX
        , El.spacing 20
        ]
        [ Input.text [ onEnter FormSubmit ]
            { text = currentUserName
            , label = Input.labelAbove [] (El.text "Please enter your name: ")
            , onChange = UserNameEntered
            , placeholder = Nothing
            }
        , El.paragraph
            [ Font.color Colors.darkRed
            , Font.size 16
            , Font.center
            , El.height <| El.px 16
            ]
            [ El.text message ]
        , primaryButton [ El.centerX ] (Just FormSubmit) "Enter"
        ]


viewPending : String -> Element Msg
viewPending currentUserName =
    El.column []
        [ Input.text []
            { text = currentUserName
            , label = Input.labelAbove [] (El.text "Please enter your name: ")
            , onChange = always NoOp
            , placeholder = Nothing
            }
        , primaryButton [ El.centerX ] Nothing "Enter"
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    El.htmlAttribute
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
