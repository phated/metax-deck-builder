module Data.CardType exposing (CardType(..), decoder, toString, toInt)

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)


type CardType
    = Character
    | Event
    | Battle


decoder : Decoder CardType
decoder =
    string |> andThen stringToCardType


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



-- Utils


stringToCardType : String -> Decoder CardType
stringToCardType cardType =
    case cardType of
        "Character" ->
            succeed Character

        "Event" ->
            succeed Event

        "Battle" ->
            succeed Battle

        _ ->
            fail "Invalid card type."
