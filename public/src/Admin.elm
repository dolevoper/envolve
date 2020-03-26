port module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Url exposing (Url, Protocol(..))
import Html exposing (Html, a, button, div, text, ul, li)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)


port userJoined : (String -> msg) -> Sub msg


port userLeft : (String -> msg) -> Sub msg


port startPoll : () -> Cmd msg


port recievedVote : (Bool -> msg) -> Sub msg


type Msg
    = UserJoined String
    | UserLeft String
    | StartPoll
    | RecievedVote Bool


type alias Model =
    { userName : String
    , url : Url
    , roomId : String
    , participants : List String
    , poll : Maybe AdminPollData
    }


type alias AdminPollData =
    { yes : Int
    , no : Int
    }


init : { userName : String, url : Url, roomId : String } -> ( Model, Cmd Msg )
init { userName, url, roomId } =
    ( { userName = userName, url = url, roomId = roomId, participants = [], poll = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( UserJoined userName, _ ) ->
            ( { model | participants = model.participants ++ [ userName ] }, Cmd.none )

        ( UserLeft userName, _ ) ->
            ( { model | participants = List.filter ((/=) userName) model.participants }, Cmd.none )

        ( StartPoll, Nothing ) ->
            ( { model | poll = Just (AdminPollData 0 0) }, startPoll () )

        ( RecievedVote True, Just { yes, no } ) ->
            ( { model | poll = Just (AdminPollData (yes + 1) no) }, Cmd.none )

        ( RecievedVote False, Just { yes, no } ) ->
            ( { model | poll = Just (AdminPollData yes (no + 1)) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ userJoined UserJoined
        , userLeft UserLeft
        , recievedVote RecievedVote
        ]


view : Model -> Document Msg
view model =
    let
        inviteLink = buildInviteLink model.url model.roomId
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
viewParticipant userName = li [] [ text userName ]


viewAdminPollSection : Maybe AdminPollData -> Html Msg
viewAdminPollSection adminPollData =
    case adminPollData of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just { yes, no } ->
            div []
                [ text ("Yes: " ++ String.fromInt yes ++ ", No: " ++ String.fromInt no) ]


buildInviteLink : Url -> String -> String
buildInviteLink url roomId =
    let
        schema =
            case url.protocol of
                Http -> "http://"
                Https -> "https://"
        port_ = Maybe.map (String.fromInt >> String.append ":") url.port_
    in
        schema ++ url.host ++ Maybe.withDefault "" port_ ++ "/" ++ roomId

externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]
