port module Socket exposing (connect, connected, disconnected, managing, userJoined, userLeft, startPoll, pollStarting, castVote, recievedVote)

-- Connection handling ports


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port managing : (String -> msg) -> Sub msg



-- User management ports


port userJoined : (String -> msg) -> Sub msg


port userLeft : (String -> msg) -> Sub msg



-- Poll management ports


port startPoll : () -> Cmd msg


port pollStarting : (() -> msg) -> Sub msg


port castVote : Bool -> Cmd msg


port recievedVote : (Bool -> msg) -> Sub msg
