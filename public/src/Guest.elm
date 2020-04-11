module Guest exposing (Model, Msg, init, subscriptions, update, view)

import Element as El exposing (Element)
import Element.Font as Font
import Poll as Poll
import PrimaryButton exposing (primaryButton)
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


view : Session -> Model -> Element Msg
view session model =
    El.column
        [ El.width El.fill
        , El.padding 20
        , El.spacing 20
        ]
        [ El.text ("Hello " ++ (Maybe.withDefault "" <| Session.userName session))
        , Maybe.withDefault (El.text "") <| Maybe.map viewGuestPollSection model
        ]


viewGuestPollSection : GuestPollData -> Element Msg
viewGuestPollSection pollData =
    case pollData of
        NotVoted ->
            El.column
                [ El.centerX
                , El.spacing 20
                , Font.center
                ]
                [ El.paragraph [] [ El.text "Please vote:" ]
                , El.row
                    [ El.spacing 20
                    , Font.size 70
                    ]
                    [ primaryButton [ El.padding 20 ] (Just <| VoteClicked True) "👍"
                    , primaryButton [ El.padding 20 ] (Just <| VoteClicked False) "👎"
                    ]
                ]

        Voted vote ->
            let
                voteSign =
                    if vote then
                        "👍"

                    else
                        "👎"
            in
            El.column
                [ El.centerX
                , El.spacing 20
                , Font.center
                ]
                [ El.paragraph [] [ El.text "Your vote:" ]
                , El.paragraph [ Font.size 70 ] [ El.text voteSign ]
                ]
