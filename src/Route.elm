module Route exposing (Route(..), route, fromLocation, toClassString)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parsePath, s, string)


type Route
    = Home
    | Deck
    | Card String
    | Search
    | Info


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Deck (s "deck")
        , Url.map Card (s "card" </> string)
        , Url.map Search (s "search")
        , Url.map Info (s "info")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath route location


toClassString : Route -> String
toClassString route =
    case route of
        Home ->
            "home"

        Deck ->
            "deck"

        Card _ ->
            "card"

        Search ->
            "search"

        Info ->
            "info"
