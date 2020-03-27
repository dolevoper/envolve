module UrlUtils exposing (baseUrl)

import Url exposing (Url, Protocol(..))

baseUrl : Url -> String
baseUrl url =
    let
        schema =
            case url.protocol of
                Http -> "http://"
                Https -> "https://"
        port_ = Maybe.withDefault "" (Maybe.map (String.fromInt >> String.append ":") url.port_)
    in
        schema ++ url.host ++ port_