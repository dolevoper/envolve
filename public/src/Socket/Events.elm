module Socket.Events exposing (castVote, endPoll, managing, newUser, resetPoll, startPoll, userLeft)

import Json.Decode as Decode
import Json.Encode as Encode
import Socket exposing (EmptyEvent, EventWithPayload, emptyEvent, eventWithPayload)
import Vote as Vote


startPoll : EmptyEvent p msg
startPoll =
    emptyEvent "start poll"


endPoll : EmptyEvent p msg
endPoll =
    emptyEvent "end poll"


resetPoll : EmptyEvent p msg
resetPoll =
    emptyEvent "reset poll"


castVote : EventWithPayload Vote.Vote msg
castVote =
    eventWithPayload "cast vote" Vote.encodeVote Vote.vote


managing : EventWithPayload String msg
managing =
    eventWithPayload "managing" Encode.string Decode.string


newUser : EventWithPayload String msg
newUser =
    eventWithPayload "new user" Encode.string Decode.string


userLeft : EventWithPayload String msg
userLeft =
    eventWithPayload "user left" Encode.string Decode.string
