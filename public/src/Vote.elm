module Vote exposing (Vote, decode, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type alias Vote =
    ( String, Bool )


encode : Vote -> Encode.Value
encode ( userName, vote ) =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "vote", Encode.bool vote )
        ]


decode : Encode.Value -> Result String Vote
decode value =
    let
        decodeResult =
            Decode.decodeValue decoder value
    in
    Result.mapError Decode.errorToString decodeResult


decoder : Decode.Decoder Vote
decoder =
    Decode.map2 Tuple.pair
        (Decode.field "userName" Decode.string)
        (Decode.field "vote" Decode.bool)
