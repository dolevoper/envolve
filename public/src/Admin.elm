port module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Color.OneDark as Colors
import Element as El exposing (Element)
import Element.Font as Font
import Html as Html
import Html.Attributes as Attrs
import Poll as Poll exposing (Poll, Vote)
import PrimaryButton exposing (primaryButton)
import Session exposing (Session)
import Socket as Socket
import Socket.Events exposing (castVote, endPoll, newUser, resetPoll, startPoll, userLeft)


port startScreenShare : () -> Cmd msg


port screenShareStopped : (() -> msg) -> Sub msg



-- MODEL --


type alias Model =
    { participants : List String
    , poll : Maybe Poll
    , isScreenSharing : Bool
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
    ( { participants = [], poll = Nothing, isScreenSharing = False }, Cmd.none )



-- UPDATE --


type Msg
    = UserJoined String
    | UserLeft String
    | StartPoll
    | EndPoll
    | ResetPoll
    | RecievedVote Vote
    | StartScreenShare
    | StopScreenShare
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll, model.isScreenSharing ) of
        ( UserJoined userName, Nothing, _ ) ->
            ( addParticipant userName model, Cmd.none )

        ( UserJoined userName, Just _, _ ) ->
            ( addParticipant userName model, Socket.raiseEvent startPoll.outBound )

        ( UserLeft userName, _, _ ) ->
            ( model |> removeParticipant userName |> removeVote userName
            , Cmd.none
            )

        ( StartPoll, Nothing, _ ) ->
            ( { model | poll = Just Poll.emptyPoll }, Socket.raiseEvent startPoll.outBound )

        ( EndPoll, Just _, _ ) ->
            ( { model | poll = Nothing }, Socket.raiseEvent endPoll.outBound )

        ( ResetPoll, Just _, _ ) ->
            ( { model | poll = Just Poll.emptyPoll }, Socket.raiseEvent resetPoll.outBound )

        ( RecievedVote vote, Just _, _ ) ->
            ( addVote vote model, Cmd.none )

        ( StartScreenShare, _, False ) ->
            ( { model | isScreenSharing = True }, startScreenShare () )

        ( StopScreenShare, _, True ) ->
            ( { model | isScreenSharing = False }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Socket.listen NoOp (newUser.inBound (always NoOp) UserJoined)
        , Socket.listen NoOp (userLeft.inBound (always NoOp) UserLeft)
        , Socket.listen NoOp (castVote.inBound (always NoOp) RecievedVote)
        , screenShareStopped <| always StopScreenShare
        ]



-- VIEW --


view : Session -> Model -> Element Msg
view session model =
    let
        userName =
            Maybe.withDefault "" (Session.userName session)

        inviteLink =
            Maybe.withDefault "" (Session.inviteLink session)
    in
    El.column
        [ El.spacing 20
        , El.width El.fill
        ]
        [ El.row
            [ El.width El.fill
            , El.spaceEvenly
            , El.padding 20
            ]
            [ El.text ("Hello " ++ userName)
            , El.row []
                [ El.text "Invite people to join using this link: "
                , externalLink inviteLink inviteLink
                ]
            ]
        , El.row
            [ El.width El.fill
            , El.padding 20
            , El.spacing 20
            ]
            [ viewParticipants model.participants
            , viewAdminPollSection model.poll
            ]
        , viewScreenShareSection model.isScreenSharing
        ]


viewParticipants : List String -> Element Msg
viewParticipants participants =
    El.column
        [ El.width <| El.fillPortion 1
        , El.alignTop
        , El.spacing 10
        ]
        (El.text ("Participants (" ++ String.fromInt (List.length participants) ++ "):") :: List.map (\p -> El.text <| "🙍\u{200D}♂️\t" ++ p) participants)


viewAdminPollSection : Maybe Poll -> Element Msg
viewAdminPollSection maybePoll =
    case maybePoll of
        Nothing ->
            El.el [ El.width <| El.fillPortion 2 ] <|
                primaryButton [ El.centerX ] (Just StartPoll) "Start New Poll"

        Just poll ->
            let
                yesVotes =
                    Poll.yesVotes poll

                noVotes =
                    Poll.noVotes poll
            in
            El.column
                [ El.width <| El.fillPortion 2
                , El.spacing 20
                ]
                [ El.row
                    [ El.centerX
                    , El.spacing 50
                    , Font.size 50
                    , Font.center
                    ]
                    [ El.column [ El.spacing 10 ]
                        [ El.text "👍"
                        , El.paragraph [] [ El.text <| String.fromInt yesVotes ]
                        ]
                    , El.column [ El.spacing 10 ]
                        [ El.text "👎"
                        , El.paragraph [] [ El.text <| String.fromInt noVotes ]
                        ]
                    ]
                , El.row
                    [ El.centerX
                    , El.spacing 20
                    ]
                    [ primaryButton [] (Just EndPoll) "Close Poll"
                    , primaryButton [] (Just ResetPoll) "Reset Poll"
                    ]
                ]


viewScreenShareSection : Bool -> Element Msg
viewScreenShareSection isScreenSharing =
    if not isScreenSharing then
        primaryButton [ El.centerX ] (Just StartScreenShare) "Start Screen Share"

    else
        El.el
            [ El.centerX
            , El.width <| El.px 1024
            ]
        <|
            El.html <|
                Html.video
                    [ Attrs.id "screen-share-video"
                    , Attrs.autoplay True
                    , Attrs.attribute "playsinline" ""
                    , Attrs.attribute "muted" ""
                    , Attrs.style "width" "100%"
                    , Attrs.style "object-fit" "cover"
                    ]
                    []


externalLink : String -> String -> Element Msg
externalLink url displayText =
    El.newTabLink
        [ Font.color Colors.blue
        , Font.underline
        ]
        { url = url
        , label = El.text displayText
        }
