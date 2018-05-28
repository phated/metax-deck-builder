-- TODO: Should I rename this Expansion? Set is overloaded


module Component.Card.Set
    exposing
        ( Set(..)
        , fromString
        , decoder
        , toString
        , toInt
        )

{-| Component.Card.Set represents the set a card belongs to.


# Types

@docs Set


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toInt

-}

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)
import Util exposing (decoderFromMaybe)


{-| The set of a card.
-}
type Set
    = JL
    | GL
    | AT
    | BM


{-| Create a Set from a String. Will be Nothing if the string is an invalid value.
-}
fromString : String -> Maybe Set
fromString value =
    case value of
        "JL" ->
            Just JL

        "GL" ->
            Just GL

        "AT" ->
            Just AT

        "BM" ->
            Just BM

        _ ->
            Nothing


{-| Decode a string into a Set.
-}
decoder : Decoder Set
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card set.")


{-| Renders the Set as a String.
-}
toString : Set -> String
toString cardSet =
    case cardSet of
        JL ->
            "JL"

        GL ->
            "GL"

        AT ->
            "AT"

        BM ->
            "BM"


{-| Renders the Set as an Int. Also known as an enumeration.
-}
toInt : Set -> Int
toInt set =
    case set of
        JL ->
            0

        GL ->
            1

        AT ->
            2

        BM ->
            3
