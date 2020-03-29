port module Socket exposing
    ( IncomingMessage
    , castVote
    , connect
    , connected
    , disconnected
    , endPoll
    , listen
    , managing
    , newUser
    , pollEnded
    , pollReset
    , pollStarted
    , raiseEvent
    , resetPoll
    , startPoll
    , userLeft
    , voteRecieved
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Vote as Vote



-- EVENT TYPES --


type OutEvent p
    = EmptyOutEvent String
    | OutEventWithPayload String (p -> Encode.Value) p


type InEvent p msg
    = EmptyInEvent String msg
    | InEventWithPayload String (Decode.Decoder p) (IncomingMessage p -> msg)


type alias IncomingMessage p =
    Result String p



-- OUT EVENTS --


startPoll : OutEvent p
startPoll =
    EmptyOutEvent "start poll"


endPoll : OutEvent p
endPoll =
    EmptyOutEvent "end poll"


resetPoll : OutEvent p
resetPoll =
    EmptyOutEvent "reset poll"


castVote : Vote.Vote -> OutEvent Vote.Vote
castVote =
    OutEventWithPayload "cast vote" Vote.encode



-- IN EVENTS --


managing : (Result String String -> msg) -> InEvent String msg
managing =
    InEventWithPayload "managing" Decode.string


newUser : (Result String String -> msg) -> InEvent String msg
newUser =
    InEventWithPayload "new user" Decode.string


userLeft : (Result String String -> msg) -> InEvent String msg
userLeft =
    InEventWithPayload "user left" Decode.string


pollStarted : msg -> InEvent p msg
pollStarted =
    EmptyInEvent "start poll"


pollEnded : msg -> InEvent p msg
pollEnded =
    EmptyInEvent "end poll"


pollReset : msg -> InEvent p msg
pollReset =
    EmptyInEvent "reset poll"


voteRecieved : (Result String Vote.Vote -> msg) -> InEvent Vote.Vote msg
voteRecieved =
    InEventWithPayload "cast vote" Vote.decoder



-- PORTS --


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port send : String -> Cmd msg


port sendJson : ( String, Encode.Value ) -> Cmd msg


port incomingMessage : (Event -> msg) -> Sub msg


type alias Event =
    { name : String
    , payload : Encode.Value
    }



-- EVENT HANDLING --


raiseEvent : OutEvent p -> Cmd msg
raiseEvent event =
    case event of
        EmptyOutEvent name ->
            send name

        OutEventWithPayload name encoder payload ->
            sendJson ( name, encoder payload )


listen : msg -> InEvent p msg -> Sub msg
listen noOp event =
    case event of
        EmptyInEvent name msg ->
            incomingMessage
                (\e ->
                    if e.name == name then
                        msg

                    else
                        noOp
                )

        InEventWithPayload name decoder toMsg ->
            incomingMessage
                (\e ->
                    if e.name == name then
                        Decode.decodeValue decoder e.payload |> Result.mapError Decode.errorToString |> toMsg

                    else
                        noOp
                )
