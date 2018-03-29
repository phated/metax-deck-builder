module Data.CardType exposing (CardType(..), decoder, toString)

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



-- Utils


stringToCardType : String -> Decoder CardType
stringToCardType card_type =
    case card_type of
        "Character" ->
            succeed Character

        "Event" ->
            succeed Event

        "Battle" ->
            succeed Battle

        _ ->
            fail "Invalid card type."
