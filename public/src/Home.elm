port module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text, a, button)
import Html.Attributes exposing (href, target, rel)
import Html.Events exposing (onClick)


port userJoined : (String -> msg) -> Sub msg


port managing : (String -> msg) -> Sub msg


port startPoll : () -> Cmd msg


type Msg
    = UserJoined String
    | Managing String
    | StartPoll


type Model
    = Admin AdminState
    | Guest GuestState


type alias AdminState =
    { userName : String
    , inviteLink : String
    , message : String
    }


type alias GuestState =
    { userName : String
    , message : String
    }


init : String -> ( Model, Cmd Msg )
init userName =
    ( Guest { userName = userName, message = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UserJoined userName, Admin admin ) ->
            ( Admin { admin | message = userName ++ " joined" }, Cmd.none )

        ( UserJoined userName, Guest guest ) ->
            ( Guest { guest | message = userName ++ " joined" }, Cmd.none )

        ( Managing inviteLink, Guest guest ) ->
            ( Admin { userName = guest.userName, message = guest.message, inviteLink = inviteLink }, Cmd.none )

        ( StartPoll, Admin _ ) ->
            ( model, startPoll () )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ userJoined UserJoined
        , managing Managing
        ]


view : Model -> Document Msg
view model =
    { title = "Envolve - Home"
    , body =
        case model of
            Admin admin ->
                [ viewAdmin admin ]

            Guest guest ->
                [ viewGuest guest ]
    }


viewAdmin : AdminState -> Html Msg
viewAdmin admin =
    div []
        [ div [] [ text ("Hello " ++ admin.userName) ]
        , div []
            [ text "Invite people to join using this link: "
            , externalLink admin.inviteLink admin.inviteLink
            ]
        , div [] [ text admin.message ]
        , div []
            [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]
        ]


viewGuest : GuestState -> Html Msg
viewGuest guest =
    div []
        [ div [] [ text ("Hello " ++ guest.userName) ]
        , div [] [ text guest.message ]
        ]


externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]