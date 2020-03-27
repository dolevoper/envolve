port module Socket exposing (connect, connected, disconnected, managing, userJoined, userLeft, recievedVote, send, sendJson, on)

import Json.Encode as Encode
import Dict as Dict

-- Connection handling ports


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port managing : (String -> msg) -> Sub msg



-- User management ports


port userJoined : (String -> msg) -> Sub msg


port userLeft : (String -> msg) -> Sub msg



-- Poll management ports


port recievedVote : (Encode.Value -> msg) -> Sub msg


port send : String -> Cmd msg


port sendJson : ( String, Encode.Value ) -> Cmd msg


port incomingMessage : (String -> msg) -> Sub msg


on : msg -> Dict.Dict String msg -> Sub msg
on  toNoEvent eventHandlers =
    let
        getEventHandler eventName = Maybe.withDefault toNoEvent (Dict.get eventName eventHandlers)
    in
    incomingMessage getEventHandler