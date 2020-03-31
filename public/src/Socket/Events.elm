module Socket.Events exposing (castVote, endPoll, managing, newUser, resetPoll, startPoll, userLeft)

import Json.Decode as Decode
import Json.Encode as Encode
import Poll as Poll exposing (Vote)
import RoomId as RoomId exposing (RoomId)
import Socket exposing (EmptyEvent, EventWithPayload, emptyEvent, eventWithPayload)


startPoll : EmptyEvent p msg
startPoll =
    emptyEvent "start poll"


endPoll : EmptyEvent p msg
endPoll =
    emptyEvent "end poll"


resetPoll : EmptyEvent p msg
resetPoll =
    emptyEvent "reset poll"


castVote : EventWithPayload Vote msg
castVote =
    eventWithPayload "cast vote" Poll.encodeVote Poll.vote


managing : EventWithPayload RoomId msg
managing =
    eventWithPayload "managing" RoomId.encode RoomId.decoder


newUser : EventWithPayload String msg
newUser =
    eventWithPayload "new user" Encode.string Decode.string


userLeft : EventWithPayload String msg
userLeft =
    eventWithPayload "user left" Encode.string Decode.string
