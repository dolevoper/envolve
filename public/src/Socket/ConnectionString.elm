module Socket.ConnectionString exposing (ConnectionString, fromUrl, toString)

import Url exposing (Url)
import Url.Builder as Builder
import UrlUtils exposing (baseUrl)


type ConnectionString
    = ConnectionString String String (Maybe String)


fromUrl : Url -> String -> Maybe String -> ConnectionString
fromUrl url userName roomId =
    ConnectionString (baseUrl url) userName roomId


toString : ConnectionString -> String
toString (ConnectionString baseUrl userName maybeRoomId) =
    let
        roomIdQuery =
            Maybe.withDefault [] (Maybe.map (Builder.string "roomId" >> List.singleton) maybeRoomId)
    in
    Builder.crossOrigin baseUrl [] (roomIdQuery ++ [ Builder.string "userName" userName ])
