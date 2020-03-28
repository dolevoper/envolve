module Guest exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Socket as Socket
import Vote as Vote
import Dict as Dict


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

        ( VoteClicked vote, Just _ ) ->
            let
                voteJson = Vote.encode ( model.userName, vote )
            in
            ( { model | poll = Just (Voted vote) }, Socket.sendJson ( "cast vote", voteJson ) )

        _ ->
            ( model, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions _ =
    Socket.on NoOp NoOp
        ( Dict.fromList
            [ ( "start poll", Socket.EmptyEvent PollStarting )
            , ( "end poll", Socket.EmptyEvent PollEnded )
            , ( "reset poll", Socket.EmptyEvent PollReset )
            ]
        )



view : Model -> Document Msg
view model =
    { title = "Envolve - Home"
    , body =
        [ div []
            [ div [] [ text ("Hello " ++ model.userName) ]
            , Maybe.withDefault (text "") (Maybe.map viewGuestPollSection model.poll)
            ]
        ]
    }


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
