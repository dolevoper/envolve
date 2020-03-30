module Guest exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Socket as Socket
import Socket.Events exposing (startPoll, endPoll, resetPoll, castVote)
import Vote as Vote


type Msg
    = PollStarting
    | PollEnded
    | PollReset
    | VoteClicked Bool
    | NoOp


type alias Model =
    { userName : String
    , poll : Maybe GuestPollData
    }


type GuestPollData
    = NotVoted
    | Voted Bool


init : String -> ( Model, Cmd Msg )
init userName =
    ( { userName = userName, poll = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( PollStarting, Nothing ) ->
            ( { model | poll = Just NotVoted }, Cmd.none )

        ( PollEnded, Just _ ) ->
            ( { model | poll = Nothing }, Cmd.none )

        ( PollReset, Just _ ) ->
            ( { model | poll = Just NotVoted }, Cmd.none )

        ( VoteClicked value, Just _ ) ->
            let
                vote =
                    Vote.createVote model.userName value
            in
            ( { model | poll = Just (Voted value) }, Socket.raiseEvent (castVote.outBound vote) )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (startPoll.inBound PollStarting)
        , Socket.listen NoOp (endPoll.inBound PollEnded)
        , Socket.listen NoOp (resetPoll.inBound PollReset)
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Hello " ++ model.userName) ]
        , Maybe.withDefault (text "") (Maybe.map viewGuestPollSection model.poll)
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
