port module Socket exposing
    ( EmptyEvent
    , EventWithPayload
    , IncomingMessage
    , connected
    , disconnected
    , emptyEvent
    , eventWithPayload
    , listen
    , openConnection
    , raiseEvent
    , socketError
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Socket.ConnectionString as Conn



-- EVENT TYPES --


type OutEvent p
    = EmptyOutEvent String
    | OutEventWithPayload String (p -> Encode.Value) p


type InEvent p msg
    = EmptyInEvent String msg
    | InEventWithPayload String (Decode.Decoder p) (IncomingMessage p -> msg)


type alias IncomingMessage p =
    Result String p


type alias EmptyEvent p msg =
    { outBound : OutEvent p
    , inBound : msg -> InEvent p msg
    }


type alias EventWithPayload p msg =
    { outBound : p -> OutEvent p
    , inBound : (IncomingMessage p -> msg) -> InEvent p msg
    }


emptyEvent : String -> EmptyEvent p msg
emptyEvent name =
    { outBound = EmptyOutEvent name, inBound = EmptyInEvent name }


eventWithPayload : String -> (p -> Encode.Value) -> Decode.Decoder p -> EventWithPayload p msg
eventWithPayload name encoder decoder =
    { outBound = OutEventWithPayload name encoder, inBound = InEventWithPayload name decoder }



-- CONNECTION MANAGEMENT --


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port socketError : (String -> msg) -> Sub msg


openConnection : Conn.ConnectionString -> Cmd msg
openConnection =
    Conn.toString >> connect



-- EVENT HANDLING --


port send : String -> Cmd msg


port sendJson : ( String, Encode.Value ) -> Cmd msg


port incomingMessage : (Event -> msg) -> Sub msg


type alias Event =
    { name : String
    , payload : Encode.Value
    }


raiseEvent : OutEvent p -> Cmd msg
raiseEvent event =
    case event of
        EmptyOutEvent name ->
            send name

        OutEventWithPayload name encoder payload ->
            sendJson ( name, encoder payload )


listen : msg -> InEvent p msg -> Sub msg
listen noOp event =
    case event of
        EmptyInEvent name msg ->
            incomingMessage
                (\e ->
                    if e.name == name then
                        msg

                    else
                        noOp
                )

        InEventWithPayload name decoder toMsg ->
            incomingMessage
                (\e ->
                    if e.name == name then
                        Decode.decodeValue decoder e.payload |> Result.mapError Decode.errorToString |> toMsg

                    else
                        noOp
                )
