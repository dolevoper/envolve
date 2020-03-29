module Vote exposing (Poll, Vote, createVote, emptyPoll, encodeVote, insertVote, noVotes, removeVote, vote, yesVotes)

import Json.Decode as Decode
import Json.Encode as Encode
import Set as Set



-- VOTE --


type Vote
    = Vote String Bool


createVote : String -> Bool -> Vote
createVote =
    Vote


encodeVote : Vote -> Encode.Value
encodeVote (Vote userName value) =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "vote", Encode.bool value )
        ]


vote : Decode.Decoder Vote
vote =
    Decode.map2 Vote
        (Decode.field "userName" Decode.string)
        (Decode.field "vote" Decode.bool)



-- POLL --


type Poll
    = Poll (Set.Set String) (Set.Set String)


emptyPoll : Poll
emptyPoll =
    Poll Set.empty Set.empty


yesVotes : Poll -> Int
yesVotes (Poll yes _) =
    Set.size yes


noVotes : Poll -> Int
noVotes (Poll _ no) =
    Set.size no


insertVote : Vote -> Poll -> Poll
insertVote (Vote userName value) (Poll yes no) =
    if value then
        Poll (Set.insert userName yes) (Set.remove userName no)

    else
        Poll (Set.remove userName yes) (Set.insert userName no)


removeVote : String -> Poll -> Poll
removeVote userName (Poll yes no) =
    Poll (Set.remove userName yes) (Set.remove userName no)
