module Route exposing (Route(..), route, fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parsePath, s, string)

type Route
    = Home
    | Deck
    | Card String


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Deck (s "deck")
        , Url.map Card (s "card" </> string)
        ]

fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath route location
