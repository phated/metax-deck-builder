module Data.Deck exposing (Deck, decoder, increment, decrement)

import Dict exposing (Dict)
import Json.Decode exposing (int, string, decodeValue, decodeString, dict, Decoder, Value)


type alias Deck =
    Dict String Int


decoder : Value -> Deck
decoder session =
    session
        |> decodeValue string
        |> Result.andThen (decodeString (dict int))
        |> Result.withDefault Dict.empty


increment : String -> Deck -> Deck
increment cardId deck =
    Dict.update cardId maybeIncrement deck


decrement : String -> Deck -> Deck
decrement cardId deck =
    let
        updatedDeck =
            Dict.update cardId maybeDecrement deck
    in
        Dict.filter notZero updatedDeck



-- Utils


notZero : String -> Int -> Bool
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
