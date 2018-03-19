module Route exposing (Route(..), route, fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parsePath, s, string, stringParam)


type Route
    = Home (Maybe String)
    | Deck (Maybe String)
    | Card String (Maybe String)
    | Search (Maybe String)


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "" <?> stringParam "deck")
        , Url.map Deck (s "deck" <?> stringParam "deck")
        , Url.map Card (s "card" </> string <?> stringParam "deck")
        , Url.map Search (s "search" <?> stringParam "deck")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath route location
