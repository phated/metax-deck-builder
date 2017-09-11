module Data.CardType exposing (CardType(..), decoder, stringToCardType, cardTypeToString)

import Json.Decode exposing (string, andThen, Decoder)
import Json.Decode.Extra exposing (fromResult)


type CardType
    = Character
    | Event
    | Battle


decoder : Decoder CardType
decoder =
    string |> andThen (fromString >> fromResult)

fromString : String -> Result String CardType
fromString =
    Result.fromMaybe "Invalid card type." << stringToCardType


-- Utils
stringToCardType : String -> Maybe CardType
stringToCardType card_type =
    case card_type of
        "Character" ->
            Just Character

        "Event" ->
            Just Event

        "Battle" ->
            Just Battle

        _ ->
            Nothing


cardTypeToString : CardType -> String
cardTypeToString cardType =
    case cardType of
        Character ->
            "Character"

        Event ->
            "Event"

        Battle ->
            "Battle"
