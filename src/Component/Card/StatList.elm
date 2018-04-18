module Component.Card.StatList
    exposing
        ( StatList
        , decoder
        , toHtml
        )

{-| Component.Card.StatList represents all stats of a card.


# Types

@docs StatList


# Build


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (Html)
import Json.Decode exposing (Decoder, list)
import Component.Card.Stat as CardStat exposing (Stat)


{-| A list containing all stats of a card.
-}
type alias StatList =
    List Stat


{-| Decodes a string into a StatList
-}
decoder : Decoder StatList
decoder =
    list CardStat.decoder


{-| Renders a StatList to a list of Html views.

TODO: This return signature messes up lazy rendering

-}
toHtml : StatList -> List (Html msg)
toHtml stats =
    List.map CardStat.toHtmlLazy stats
