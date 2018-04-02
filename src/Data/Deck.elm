module Data.Deck exposing (Deck, decoder, increment, decrement, empty, toList, count)

import AllDict exposing (AllDict)
import Json.Decode exposing (int, string, decodeValue, decodeString, dict, Decoder, Value)
import Data.Card exposing (Card)


type alias Deck =
    AllDict Card Int String


empty : Deck
empty =
    AllDict.empty toComparable


toList : Deck -> List ( Card, Int )
toList deck =
    AllDict.toList deck


count : Card -> Deck -> Int
count card deck =
    AllDict.get card deck
        |> Maybe.withDefault 0


decoder : Value -> Deck
decoder session =
    -- session
    --     |> decodeValue string
    --     |> Result.andThen (decodeString (dict int))
    --     |> Result.withDefault empty
    empty


increment : Card -> Deck -> Deck
increment card deck =
    AllDict.update card maybeIncrement deck


decrement : Card -> Deck -> Deck
decrement card deck =
    let
        updatedDeck =
            AllDict.update card maybeDecrement deck
    in
        AllDict.filter notZero updatedDeck



-- Utils


toComparable : Card -> String
toComparable card =
    card.uid


notZero : Card -> Int -> Bool
notZero _ count =
    count /= 0


maybeIncrement : Maybe Int -> Maybe Int
maybeIncrement value =
    case value of
        Just value ->
            let
                val =
                    if value < 3 then
                        value + 1
                    else
                        value
            in
                Just val

        Nothing ->
            Just 1


maybeDecrement : Maybe Int -> Maybe Int
maybeDecrement value =
    case value of
        Just value ->
            let
                val =
                    if value > 0 then
                        value - 1
                    else
                        value
            in
                Just val

        Nothing ->
            Just 0
