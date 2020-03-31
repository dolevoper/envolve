module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)
import Poll as Poll exposing (Poll, Vote)
import Session exposing (Session)
import Socket as Socket
import Socket.Events exposing (castVote, endPoll, newUser, resetPoll, startPoll, userLeft)



-- MODEL --


type alias Model =
    { participants : List String
    , poll : Maybe Poll
    }


addParticipant : String -> Model -> Model
addParticipant userName model =
    { model | participants = userName :: model.participants }


addVote : Vote -> Model -> Model
addVote vote model =
    { model | poll = Maybe.map (Poll.insertVote vote) model.poll }


removeParticipant : String -> Model -> Model
removeParticipant userName m =
    { m | participants = List.filter ((/=) userName) m.participants }


removeVote : String -> Model -> Model
removeVote userName m =
    { m | poll = Maybe.map (Poll.removeVote userName) m.poll }


init : ( Model, Cmd Msg )
init =
    ( { participants = [], poll = Nothing }, Cmd.none )



-- UPDATE --


type Msg
    = UserJoined String
    | UserLeft String
    | StartPoll
    | EndPoll
    | ResetPoll
    | RecievedVote Vote
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( UserJoined userName, Nothing ) ->
            ( addParticipant userName model, Cmd.none )

        ( UserJoined userName, Just _ ) ->
            ( addParticipant userName model, Socket.raiseEvent startPoll.outBound )

        ( UserLeft userName, _ ) ->
            ( model |> removeParticipant userName |> removeVote userName
            , Cmd.none
            )

        ( StartPoll, Nothing ) ->
            ( { model | poll = Just Poll.emptyPoll }, Socket.raiseEvent startPoll.outBound )

        ( EndPoll, Just _ ) ->
            ( { model | poll = Nothing }, Socket.raiseEvent endPoll.outBound )

        ( ResetPoll, Just _ ) ->
            ( { model | poll = Just Poll.emptyPoll }, Socket.raiseEvent resetPoll.outBound )

        ( RecievedVote vote, Just _ ) ->
            ( addVote vote model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (newUser.inBound (always NoOp) UserJoined)
        , Socket.listen NoOp (userLeft.inBound (always NoOp) UserLeft)
        , Socket.listen NoOp (castVote.inBound (always NoOp) RecievedVote)
        ]



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        userName =
            Maybe.withDefault "" (Session.userName session)

        inviteLink =
            Maybe.withDefault "" (Session.inviteLink session)
    in
    div []
        [ div [] [ text ("Hello " ++ userName) ]
        , div []
            [ text "Invite people to join using this link: "
            , externalLink inviteLink inviteLink
            ]
        , viewParticipants model.participants
        , viewAdminPollSection model.poll
        ]


viewParticipants : List String -> Html Msg
viewParticipants participants =
    div []
        [ text ("Participants (" ++ String.fromInt (List.length participants) ++ "):")
        , ul [] (List.map viewParticipant participants)
        ]


viewParticipant : String -> Html Msg
viewParticipant userName =
    li [] [ text userName ]


viewAdminPollSection : Maybe Poll -> Html Msg
viewAdminPollSection maybePoll =
    case maybePoll of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just poll ->
            let
                yesVotes =
                    Poll.yesVotes poll

                noVotes =
                    Poll.noVotes poll
            in
            div []
                [ text ("Yes: " ++ String.fromInt yesVotes ++ ", No: " ++ String.fromInt noVotes)
                , button [ onClick EndPoll ] [ text "Close Poll" ]
                , button [ onClick ResetPoll ] [ text "Reset Poll" ]
                ]


externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]
