port module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)


port userJoined : (String -> msg) -> Sub msg


port startPoll : () -> Cmd msg


port recievedVote : (Bool -> msg) -> Sub msg


type Msg
    = UserJoined String
    | StartPoll
    | RecievedVote Bool


type alias Model =
    { userName : String
    , inviteLink : String
    , message : String
    , poll : Maybe AdminPollData
    }


type alias AdminPollData =
    { yes : Int
    , no : Int
    }


init : { userName : String, inviteLink : String } -> ( Model, Cmd Msg )
init { userName, inviteLink } =
    ( { userName = userName, inviteLink = inviteLink, message = "", poll = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.poll ) of
        ( UserJoined userName, _ ) ->
            ( { model | message = userName ++ " joined" }, Cmd.none )

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
        , recievedVote RecievedVote
        ]


view : Model -> Document Msg
view model =
    { title = "Envolve - Home"
    , body =
        [ div []
            [ div [] [ text ("Hello " ++ model.userName) ]
            , div []
                [ text "Invite people to join using this link: "
                , externalLink model.inviteLink model.inviteLink
                ]
            , div [] [ text model.message ]
            , viewAdminPollSection model.poll
            ]
        ]
    }


viewAdminPollSection : Maybe AdminPollData -> Html Msg
viewAdminPollSection adminPollData =
    case adminPollData of
        Nothing ->
            div []
                [ button [ onClick StartPoll ] [ text "Start New Poll" ] ]

        Just { yes, no } ->
            div []
                [ text ("Yes: " ++ String.fromInt yes ++ ", No: " ++ String.fromInt no) ]


externalLink : String -> String -> Html Msg
externalLink url displayText =
    a [ href url, target "_blank", rel "noopener noreferrer" ] [ text displayText ]
