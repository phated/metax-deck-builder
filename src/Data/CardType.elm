module Data.CardType
    exposing
        ( CardType(..)
        , decoder
        , toString
        , fromString
        , toInt
        )

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)
import Util exposing (decoderFromMaybe)


type CardType
    = Character
    | Event
    | Battle


decoder : Decoder CardType
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card type.")


fromString : String -> Maybe CardType
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


toString : CardType -> String
toString cardType =
    case cardType of
        Character ->
            "Character"

        Event ->
            "Event"

        Battle ->
            "Battle"


toInt : CardType -> Int
toInt cardType =
    case cardType of
        Character ->
            1

        Battle ->
            2

        Event ->
            3
