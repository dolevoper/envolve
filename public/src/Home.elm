port module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)


port userJoined : (String -> msg) -> Sub msg


port managing : (String -> msg) -> Sub msg


port startPoll : () -> Cmd msg


port pollStarting : (() -> msg) -> Sub msg


type Msg
    = UserJoined String
    | Managing String
    | StartPoll
    | PollStarting
    | VoteClicked Bool


type Model
    = Admin AdminState
    | Guest GuestState


type alias AdminState =
    { userName : String
    , inviteLink : String
    , message : String
    , poll : Maybe AdminPollData
    }


type alias AdminPollData =
    { yes : Int
    , no : Int
    }


type alias GuestState =
    { userName : String
    , poll : Maybe GuestPollData
    }


type GuestPollData
    = NotVoted
    | Voted Bool


init : String -> ( Model, Cmd Msg )
init userName =
    ( Guest { userName = userName, poll = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UserJoined userName, Admin admin ) ->
            ( Admin { admin | message = userName ++ " joined" }, Cmd.none )

        ( StartPoll, Admin admin ) ->
            ( Admin { admin | poll = Just (AdminPollData 0 0) }, startPoll () )

        ( Managing inviteLink, Guest guest ) ->
            ( Admin { userName = guest.userName, message = "", inviteLink = inviteLink, poll = Nothing }, Cmd.none )

        ( PollStarting, Guest guest ) ->
            ( Guest { guest | poll = Just NotVoted }, Cmd.none )

        ( VoteClicked vote, Guest guest ) ->
            ( Guest { guest | poll = Just (Voted vote) }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ userJoined UserJoined
        , managing Managing
        , pollStarting (always PollStarting)
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
        , viewAdminPollSection admin.poll
        ]


viewAdminPollSection : Maybe AdminPollData -> Html Msg
viewAdminPollSection adminPollData =
    case adminPollData of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just { yes, no } ->
            div []
                [ text ("Yes: " ++ String.fromInt yes ++ ", No: " ++ String.fromInt no) ]


viewGuest : GuestState -> Html Msg
viewGuest guest =
    div []
        [ div [] [ text ("Hello " ++ guest.userName) ]
        , Maybe.withDefault (text "") (Maybe.map viewGuestPollSection guest.poll)
        ]


viewGuestPollSection : GuestPollData -> Html Msg
viewGuestPollSection pollData =
    case pollData of
        NotVoted ->
            div []
                [ text "Please vote: "
                , button [ onClick (VoteClicked True) ] [ text "Yes" ]
                , button [ onClick (VoteClicked False) ] [ text "No" ]
                ]

        Voted vote ->
            div []
                [ text ("Your vote: " ++ if vote then "Yes" else "No") ]


externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]
