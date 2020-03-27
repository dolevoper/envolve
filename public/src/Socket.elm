port module Socket exposing (connect, connected, disconnected, managing, userJoined, userLeft, pollStarting, pollEnded, recievedVote, send, sendJson)

import Json.Encode as Encode

-- Connection handling ports


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port managing : (String -> msg) -> Sub msg



-- User management ports


port userJoined : (String -> msg) -> Sub msg


port userLeft : (String -> msg) -> Sub msg



-- Poll management ports


port pollStarting : (() -> msg) -> Sub msg


port pollEnded : (() -> msg) -> Sub msg


port recievedVote : (Encode.Value -> msg) -> Sub msg


port send : String -> Cmd msg


port sendJson : ( String, Encode.Value ) -> Cmd msg