module Route exposing (Route(..), fromUrl, roomId, toString)

import RoomId as RoomId exposing (RoomId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = CreateRoom
    | JoinRoom RoomId
    | ManageRoom RoomId
    | ViewRoom RoomId
    | PresentRoom RoomId
    | NotFound


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map CreateRoom Parser.top
        , Parser.map JoinRoom (RoomId.urlParser </> Parser.s "join")
        , Parser.map ManageRoom (RoomId.urlParser </> Parser.s "manage")
        , Parser.map ViewRoom (RoomId.urlParser </> Parser.s "view")
        , Parser.map PresentRoom (RoomId.urlParser </> Parser.s "present")
        ]


fromUrl : Url -> Route
fromUrl =
    Parser.parse parser >> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    let
        toPath parts =
            "/" ++ String.join "/" parts
    in
    case route of
        CreateRoom ->
            "/"

        JoinRoom id ->
            toPath [ RoomId.toString id, "join" ]

        ManageRoom id ->
            toPath [ RoomId.toString id, "manage" ]

        ViewRoom id ->
            toPath [ RoomId.toString id, "view" ]

        PresentRoom id ->
            toPath [ RoomId.toString id, "present" ]

        NotFound ->
            "/"


roomId : Route -> Maybe RoomId
roomId route =
    case route of
        CreateRoom ->
            Nothing

        JoinRoom id ->
            Just id

        ManageRoom id ->
            Just id

        ViewRoom id ->
            Just id

        PresentRoom id ->
            Just id

        NotFound ->
            Nothing
