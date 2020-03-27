port module Socket exposing (connect, connected, disconnected, managing, userJoined, userLeft, startPoll, pollStarting, endPoll, pollEnded, castVote, recievedVote)

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


port endPoll : () -> Cmd msg


port pollEnded : (() -> msg) -> Sub msg


port castVote : ( String, Bool ) -> Cmd msg


port recievedVote : (( String, Bool ) -> msg) -> Sub msg
