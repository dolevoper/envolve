module Guest exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Poll as Poll
import Session as Session exposing (Session)
import Socket as Socket
import Socket.Events exposing (castVote, endPoll, resetPoll, startPoll)


type Msg
    = PollStarting
    | PollEnded
    | PollReset
    | VoteClicked Bool
    | NoOp


type alias Model =
    Maybe GuestPollData


type GuestPollData
    = NotVoted
    | Voted Bool


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( msg, model ) of
        ( PollStarting, Nothing ) ->
            ( Just NotVoted, Cmd.none )

        ( PollEnded, Just _ ) ->
            ( Nothing, Cmd.none )

        ( PollReset, Just _ ) ->
            ( Just NotVoted, Cmd.none )

        ( VoteClicked value, Just _ ) ->
            let
                userName =
                    Maybe.withDefault "" <| Session.userName session

                vote =
                    Poll.createVote userName value
            in
            ( Just (Voted value), Socket.raiseEvent (castVote.outBound vote) )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (startPoll.inBound PollStarting)
        , Socket.listen NoOp (endPoll.inBound PollEnded)
        , Socket.listen NoOp (resetPoll.inBound PollReset)
        ]


view : Session -> Model -> Html Msg
view session model =
    div []
        [ div [] [ text ("Hello " ++ (Maybe.withDefault "" <| Session.userName session)) ]
        , Maybe.withDefault (text "") (Maybe.map viewGuestPollSection model)
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
                [ text
                    ("Your vote: "
                        ++ (if vote then
                                "Yes"

                            else
                                "No"
                           )
                    )
                ]
