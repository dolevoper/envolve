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
    = ConnectionString BaseUrl (Maybe UserName) (Maybe RoomId)


fromUrl : Url -> Maybe UserName -> ConnectionString
fromUrl url userName =
    ConnectionString (baseUrl url) userName (url |> Route.fromUrl |> Route.roomId)


toString : ConnectionString -> String
toString (ConnectionString baseUrl userName roomId) =
    let
        roomIdQuery =
            Maybe.map (RoomId.toString >> Builder.string "roomId" >> List.singleton) roomId

        userNameQuery =
            Maybe.map (Builder.string "userName" >> List.singleton) userName

        query =
            Maybe.withDefault [] roomIdQuery ++ Maybe.withDefault [] userNameQuery
    in
    Builder.crossOrigin baseUrl [] query
