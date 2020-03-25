port module Home exposing (Msg, Model, init, update, subscriptions, view)

import Browser exposing (Document)
import Html exposing (div, text)


port userJoined : (String -> msg) -> Sub msg


type Msg
    = UserJoined String


type alias Model =
    { userName : String
    , message : String
    }


init : String -> ( Model, Cmd Msg )
init userName =
    ( { userName = userName, message = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserJoined userName ->
            ( { model | message = userName ++ " joined" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    userJoined UserJoined


view : Model -> Document Msg
view model =
    { title = "Envolve - Home"
    , body =
        [ div []
            [ text ("Hello " ++ model.userName) ]
        , div []
            [ text model.message ]
        ]
    }
