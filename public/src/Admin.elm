module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)
import Socket as Socket
import Url exposing (Protocol(..), Url)
import UrlUtils exposing (baseUrl)
import Vote as Vote



-- MODEL --


type alias Model =
    { userName : String
    , url : Url
    , roomId : String
    , participants : List String
    , poll : Maybe Vote.Poll
    }


addParticipant : String -> Model -> Model
addParticipant userName model =
    { model | participants = userName :: model.participants }


addVote : Vote.Vote -> Model -> Model
addVote vote model =
    { model | poll = Maybe.map (Vote.insertVote vote) model.poll }


removeParticipant : String -> Model -> Model
removeParticipant userName m =
    { m | participants = List.filter ((/=) userName) m.participants }


removeVote : String -> Model -> Model
removeVote userName m =
    { m | poll = Maybe.map (Vote.removeVote userName) m.poll }


init : { userName : String, url : Url, roomId : String } -> ( Model, Cmd Msg )
init { userName, url, roomId } =
    ( { userName = userName, url = url, roomId = roomId, participants = [], poll = Nothing }, Cmd.none )



-- UPDATE --


type Msg
    = UserJoined (Socket.IncomingMessage String)
    | UserLeft (Socket.IncomingMessage String)
    | StartPoll
    | EndPoll
    | ResetPoll
    | RecievedVote (Socket.IncomingMessage Vote.Vote)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( UserJoined (Ok userName), Nothing ) ->
            ( addParticipant userName model, Cmd.none )

        ( UserJoined (Ok userName), Just _ ) ->
            ( addParticipant userName model, Socket.raiseEvent Socket.startPoll )

        ( UserLeft (Ok userName), _ ) ->
            ( model |> removeParticipant userName |> removeVote userName
            , Cmd.none
            )

        ( StartPoll, Nothing ) ->
            ( { model | poll = Just Vote.emptyPoll }, Socket.raiseEvent Socket.startPoll )

        ( EndPoll, Just _ ) ->
            ( { model | poll = Nothing }, Socket.raiseEvent Socket.endPoll )

        ( ResetPoll, Just _ ) ->
            ( { model | poll = Just Vote.emptyPoll }, Socket.raiseEvent Socket.resetPoll )

        ( RecievedVote (Ok vote), Just _ ) ->
            ( addVote vote model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (Socket.newUser UserJoined)
        , Socket.listen NoOp (Socket.userLeft UserLeft)
        , Socket.listen NoOp (Socket.voteRecieved RecievedVote)
        ]



-- VIEW --


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
