module Component.Card.StatType
    exposing
        ( StatType(..)
        , fromString
        , decoder
        , toString
        , toHtml
        , order
        )

{-| Component.Card.StatType represents the type a stat can be.


# Types

@docs StatType


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toHtml


# Ordering

@docs order

-}

import Html exposing (Html, img)
import Html.Attributes exposing (src, class)
import Json.Decode exposing (Decoder, string, andThen)
import Util exposing (decoderFromMaybe)


{-| The type of a stat.
-}
type StatType
    = Strength
    | Intelligence
    | Special


{-| Create a StatType from a String. Will be Nothing if the string is an invalid value.
-}
fromString : String -> Maybe StatType
fromString value =
    case value of
        "Strength" ->
            Just Strength

        "Intelligence" ->
            Just Intelligence

        "Special" ->
            Just Special

        _ ->
            Nothing


{-| Decode a string into a StatType.
-}
decoder : Decoder StatType
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid stat type.")


{-| Renders the StatType as a String.
-}
toString : StatType -> String
toString statType =
    -- Using lowercase here because this is used to generate an icon for MultiStat
    case statType of
        Strength ->
            "strength"

        Intelligence ->
            "intelligence"

        Special ->
            "special"


{-| Renders the StatType as an Html view.
-}
toHtml : StatType -> Html msg
toHtml statType =
    case statType of
        Strength ->
            img [ class "card-stat-icon", src "/icons/strength.png" ] []

        Intelligence ->
            img [ class "card-stat-icon", src "/icons/intelligence.png" ] []

        Special ->
            img [ class "card-stat-icon", src "/icons/special.png" ] []


{-| Convert a StatType into a comparable. This is useful when used with `Compare.by`.
-}
order : StatType -> Int
order statType =
    case statType of
        Strength ->
            0

        Intelligence ->
            7

        Special ->
            14
