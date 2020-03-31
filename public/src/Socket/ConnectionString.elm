module Socket.ConnectionString exposing (ConnectionString, fromUrl, toString)

import RoomId as RoomId exposing (RoomId)
import Route as Route
import Url exposing (Url)
import Url.Builder as Builder
import UrlUtils exposing (baseUrl)


type alias BaseUrl =
    String


type alias UserName =
    String


type ConnectionString
    = ConnectionString BaseUrl UserName (Maybe RoomId)


fromUrl : Url -> UserName -> ConnectionString
fromUrl url userName =
    ConnectionString (baseUrl url) userName (url |> Route.fromUrl |> Route.roomId)


toString : ConnectionString -> String
toString (ConnectionString baseUrl userName roomId) =
    let
        roomIdQuery =
            Maybe.map (RoomId.toString >> Builder.string "roomId") roomId

        query =
            Maybe.withDefault [] (Maybe.map List.singleton roomIdQuery)
    in
    Builder.crossOrigin baseUrl [] (Builder.string "userName" userName :: query)
