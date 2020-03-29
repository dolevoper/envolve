port module Socket exposing (Message, SocketEvent, SocketEventHandler(..), connect, connected, disconnected, raiseEvent, eventPayloadHandler, on, sendJson)

import Dict as Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Socket.Events as SocketEvents



-- Connection handling ports


port connect : String -> Cmd msg


port connected : (() -> msg) -> Sub msg


port disconnected : (() -> msg) -> Sub msg


port send : String -> Cmd msg


port sendJson : ( String, Encode.Value ) -> Cmd msg


port incomingMessage : (SocketEvent -> msg) -> Sub msg


type alias SocketEvent =
    { name : String
    , payload : Maybe Encode.Value
    }


type SocketEventHandler msg
    = EmptyEvent msg
    | EventWithPayload (Encode.Value -> msg)


type alias Message payload =
    Result String payload


raiseEvent : SocketEvents.Event -> Cmd msg
raiseEvent = SocketEvents.eventName >> send


on : msg -> msg -> Dict.Dict String (SocketEventHandler msg) -> Sub msg
on unhandledEvent socketErrorEvent eventHandlers =
    let
        getEventHandler : String -> Maybe (SocketEventHandler msg)
        getEventHandler eventName =
            Dict.get eventName eventHandlers

        handleEvent : Maybe (SocketEventHandler msg) -> SocketEvent -> msg
        handleEvent handler event =
            case ( handler, event.payload ) of
                ( Nothing, _ ) ->
                    unhandledEvent

                ( Just (EmptyEvent message), _ ) ->
                    message

                ( Just (EventWithPayload _), Nothing ) ->
                    socketErrorEvent

                ( Just (EventWithPayload toMsg), Just payload ) ->
                    toMsg payload
    in
    incomingMessage
        (\event ->
            let
                handler =
                    getEventHandler event.name
            in
            handleEvent handler event
        )


eventPayloadHandler : Decode.Decoder payload -> (Message payload -> msg) -> Encode.Value -> msg
eventPayloadHandler decoder toMsg =
    Decode.decodeValue decoder >> Result.mapError Decode.errorToString >> toMsg
