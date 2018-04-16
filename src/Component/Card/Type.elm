-- TODO: Should I rename this? Type is a very overloaded term


module Component.Card.Type
    exposing
        ( Type(..)
        , fromString
        , decoder
        , toString
        , toInt
        )

{-| Component.Card.Type represents the type of the card (e.g. Character/Event/Battle).


# Types

@docs Type


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toInt

-}

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)
import Util exposing (decoderFromMaybe)


{-| The type of a card.
-}
type Type
    = Character
    | Event
    | Battle


{-| Create a Type from a String. Will be Nothing if the string is an invalid value.
-}
fromString : String -> Maybe Type
fromString value =
    case value of
        "Character" ->
            Just Character

        "Event" ->
            Just Event

        "Battle" ->
            Just Battle

        _ ->
            Nothing


{-| Decode a string into a Type.
-}
decoder : Decoder Type
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card type.")


{-| Renders the Type as a String.
-}
toString : Type -> String
toString cardType =
    case cardType of
        Character ->
            "Character"

        Event ->
            "Event"

        Battle ->
            "Battle"


{-| Renders the Type as an Int. Also known as an enumeration.
-}
toInt : Type -> Int
toInt cardType =
    case cardType of
        Character ->
            1

        Battle ->
            2

        Event ->
            3
