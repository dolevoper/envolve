module Session exposing (Session, inviteLink, isLoggedIn, login, newSession, presentationLink, pushUrl, updateUrl, url, userName)

import Browser.Navigation as Nav
import RoomId exposing (RoomId)
import Route
import Url exposing (Url)
import UrlUtils exposing (baseUrl)


type alias UserName =
    String


type Session
    = NotLoggedIn
        { url : Url
        , key : Nav.Key
        }
    | LoggedIn
        { url : Url
        , key : Nav.Key
        , roomId : RoomId
        , userName : UserName
        }


newSession : Url -> Nav.Key -> Session
newSession u k =
    NotLoggedIn { url = u, key = k }


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        NotLoggedIn _ ->
            False

        LoggedIn _ ->
            True


key : Session -> Nav.Key
key session =
    case session of
        NotLoggedIn s ->
            s.key

        LoggedIn s ->
            s.key


pushUrl : Session -> String -> Cmd msg
pushUrl session u =
    Nav.pushUrl (key session) u


inviteLink : Session -> Maybe String
inviteLink session =
    case session of
        LoggedIn s ->
            Just (baseUrl s.url ++ Route.toString (Route.JoinRoom s.roomId))

        NotLoggedIn _ ->
            Nothing


presentationLink : Session -> Maybe String
presentationLink session =
    case session of
        LoggedIn s ->
            Just (baseUrl s.url ++ Route.toString (Route.PresentRoom s.roomId))

        NotLoggedIn _ ->
            Nothing


userName : Session -> Maybe UserName
userName session =
    case session of
        NotLoggedIn _ ->
            Nothing

        LoggedIn s ->
            Just s.userName


login : RoomId -> UserName -> Session -> Result String Session
login roomId u session =
    case session of
        NotLoggedIn s ->
            Ok (LoggedIn { url = s.url, key = s.key, roomId = roomId, userName = u })

        LoggedIn _ ->
            Err "you're already logged in"


url : Session -> Url
url session =
    case session of
        NotLoggedIn s ->
            s.url

        LoggedIn s ->
            s.url


updateUrl : Url -> Session -> Session
updateUrl u session =
    case session of
        LoggedIn s ->
            LoggedIn { s | url = u }

        NotLoggedIn s ->
            NotLoggedIn { s | url = u }
