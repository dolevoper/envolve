module Vote exposing (Vote, encode, decode, decoder)

import Json.Encode as Encode
import Json.Decode as Decode


type alias Vote = ( String, Bool )


encode : Vote -> Encode.Value
encode ( userName, vote ) =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "vote", Encode.bool vote )
        ]


decode : Encode.Value -> Result String Vote
decode value =
    let
        decodeResult = Decode.decodeValue decoder value
    in
    Result.mapError (always "failed parsing vote") decodeResult


decoder : Decode.Decoder Vote
decoder =
    Decode.map2 Tuple.pair
        (Decode.field "userName" Decode.string)
        (Decode.field "vote" Decode.bool)