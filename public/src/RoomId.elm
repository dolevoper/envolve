module RoomId exposing (RoomId, decoder, encode, toString, urlParser)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Url.Parser as Parser exposing (Parser)


type RoomId
    = RoomId String


urlParser : Parser (RoomId -> a) a
urlParser =
    Parser.custom "ROOM_ID" (RoomId >> Just)


toString : RoomId -> String
toString (RoomId roomId) =
    roomId



-- JSON --


encode : RoomId -> Value
encode (RoomId roomId) =
    E.string roomId


decoder : Decoder RoomId
decoder =
    D.map RoomId D.string
