module Socket.Events exposing (Event, eventName, managing, newUser, userLeft, startPoll, endPoll, resetPoll, castVote)

-- EVENT TYPE

type Event = Event String

eventName : Event -> String
eventName (Event name) = name


-- SPECIFIC EVENTS

managing : Event
managing = Event "managing"

newUser : Event
newUser = Event "new user"

userLeft : Event
userLeft = Event "user left"

startPoll : Event
startPoll = Event "start poll"

endPoll : Event
endPoll = Event "end poll"

resetPoll : Event
resetPoll = Event "rest poll"

castVote : Event
castVote = Event "cast vote"