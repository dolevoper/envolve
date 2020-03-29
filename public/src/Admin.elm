module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
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
    , poll : Maybe AdminPollData
    }


type alias AdminPollData =
    List ( String, Bool )


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
                    Maybe.map (List.filter (Tuple.first >> (/=) userName)) model.poll
            in
            ( { model | participants = participants, poll = poll }
            , Cmd.none
            )

        ( StartPoll, Nothing ) ->
            ( { model | poll = Just [] }, Socket.raiseEvent Socket.startPoll )

        ( EndPoll, Just _ ) ->
            ( { model | poll = Nothing }, Socket.raiseEvent Socket.endPoll )

        ( ResetPoll, Just _ ) ->
            ( { model | poll = Just [] }, Socket.raiseEvent Socket.resetPoll )

        ( RecievedVote (Ok vote), Just votes ) ->
            ( { model | poll = Just (votes ++ [ vote ]) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (Socket.newUser UserJoined)
        , Socket.listen NoOp (Socket.userLeft UserLeft)
        , Socket.listen NoOp (Socket.voteRecieved RecievedVote)
        ]


view : Model -> Document Msg
view model =
    let
        inviteLink =
            buildInviteLink model.url model.roomId
    in
    { title = "Envolve - Home"
    , body =
        [ div []
            [ div [] [ text ("Hello " ++ model.userName) ]
            , div []
                [ text "Invite people to join using this link: "
                , externalLink inviteLink inviteLink
                ]
            , viewParticipants model.participants
            , viewAdminPollSection model.poll
            ]
        ]
    }


viewParticipants : List String -> Html Msg
viewParticipants participants =
    div []
        [ text ("Participants (" ++ String.fromInt (List.length participants) ++ "):")
        , ul [] (List.map viewParticipant participants)
        ]


viewParticipant : String -> Html Msg
viewParticipant userName =
    li [] [ text userName ]


viewAdminPollSection : Maybe AdminPollData -> Html Msg
viewAdminPollSection adminPollData =
    case adminPollData of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just votes ->
            let
                yesVotes =
                    List.length (List.filter Tuple.second votes)

                noVotes =
                    List.length votes - yesVotes
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
