module Component.Card.Rarity
    exposing
        ( Rarity(..)
        , fromString
        , decoder
        , toString
        , toInt
        )

{-| Component.Card.Rarity represents the rarity of a card.


# Types

@docs Rarity


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toInt

-}

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)
import Util exposing (decoderFromMaybe)


{-| The rarity of a card.
-}
type Rarity
    = Common
    | Uncommon
    | Rare
    | XRare
    | URare
    | Promo
    | Starter


{-| Create a Rarity from a String. Will be Nothing if the string is an invalid value.
-}
fromString : String -> Maybe Rarity
fromString value =
    case value of
        "C" ->
            Just Common

        "U" ->
            Just Uncommon

        "R" ->
            Just Rare

        "XR" ->
            Just XRare

        "UR" ->
            Just URare

        "P" ->
            Just Promo

        "S" ->
            Just Starter

        _ ->
            Nothing


{-| Decodes a string into a Rarity.
-}
decoder : Decoder Rarity
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card rarity.")


{-| Renders the Rarity as a String.
-}
toString : Rarity -> String
toString rarity =
    case rarity of
        Common ->
            "C"

        Uncommon ->
            "U"

        Rare ->
            "R"

        XRare ->
            "XR"

        URare ->
            "UR"

        Promo ->
            "P"

        Starter ->
            "S"


{-| Renders the Rarity as an Int. Also known as an enumeration.
-}
toInt : Rarity -> Int
toInt rarity =
    case rarity of
        Starter ->
            0

        Common ->
            1

        Uncommon ->
            2

        Rare ->
            3

        Promo ->
            4

        XRare ->
            5

        URare ->
            6
