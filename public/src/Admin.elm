module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)
import Socket as Socket
import Url exposing (Protocol(..), Url)
import UrlUtils exposing (baseUrl)
import Vote as Vote


type Msg
    = UserJoined (Socket.IncomingMessage String)
    | UserLeft (Socket.IncomingMessage String)
    | StartPoll
    | EndPoll
    | ResetPoll
    | RecievedVote (Socket.IncomingMessage Vote.Vote)
    | NoOp


type alias Model =
    { userName : String
    , url : Url
    , roomId : String
    , participants : List String
    , poll : Maybe Vote.Poll
    }


init : { userName : String, url : Url, roomId : String } -> ( Model, Cmd Msg )
init { userName, url, roomId } =
    ( { userName = userName, url = url, roomId = roomId, participants = [], poll = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( UserJoined (Ok userName), Nothing ) ->
            ( { model | participants = model.participants ++ [ userName ] }, Cmd.none )

        ( UserJoined (Ok userName), Just _ ) ->
            ( { model | participants = model.participants ++ [ userName ] }, Socket.raiseEvent Socket.startPoll )

        ( UserLeft (Ok userName), _ ) ->
            let
                participants =
                    List.filter ((/=) userName) model.participants

                poll =
                    Maybe.map (Vote.removeVote userName) model.poll
            in
            ( { model | participants = participants, poll = poll }
            , Cmd.none
            )

        ( StartPoll, Nothing ) ->
            ( { model | poll = Just Vote.emptyPoll }, Socket.raiseEvent Socket.startPoll )

        ( EndPoll, Just _ ) ->
            ( { model | poll = Nothing }, Socket.raiseEvent Socket.endPoll )

        ( ResetPoll, Just _ ) ->
            ( { model | poll = Just Vote.emptyPoll }, Socket.raiseEvent Socket.resetPoll )

        ( RecievedVote (Ok vote), Just poll ) ->
            ( { model | poll = Just (Vote.insertVote vote poll) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (Socket.newUser UserJoined)
        , Socket.listen NoOp (Socket.userLeft UserLeft)
        , Socket.listen NoOp (Socket.voteRecieved RecievedVote)
        ]


view : Model -> Html Msg
view model =
    let
        inviteLink =
            buildInviteLink model.url model.roomId
    in
    div []
        [ div [] [ text ("Hello " ++ model.userName) ]
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


viewAdminPollSection : Maybe Vote.Poll -> Html Msg
viewAdminPollSection maybePoll =
    case maybePoll of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just poll ->
            let
                yesVotes =
                    Vote.yesVotes poll

                noVotes =
                    Vote.noVotes poll
            in
            div []
                [ text ("Yes: " ++ String.fromInt yesVotes ++ ", No: " ++ String.fromInt noVotes)
                , button [ onClick EndPoll ] [ text "Close Poll" ]
                , button [ onClick ResetPoll ] [ text "Reset Poll" ]
                ]


buildInviteLink : Url -> String -> String
buildInviteLink url roomId =
    baseUrl url ++ "/" ++ roomId


externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]
