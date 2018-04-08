module Data.Deck exposing (Deck, decoder, encoder, increment, decrement, empty, toList, fromList, count, sum, hash)

import AllDict exposing (AllDict)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Data.Card exposing (Card)
import Encode


type alias Deck =
    AllDict Card Int String


empty : Deck
empty =
    AllDict.empty toComparable


toList : Deck -> List ( Card, Int )
toList deck =
    AllDict.toList deck


fromList : List ( Card, Int ) -> Deck
fromList cards =
    AllDict.fromList toComparable cards


count : Card -> Deck -> Int
count card deck =
    AllDict.get card deck
        |> Maybe.withDefault 0


sum : Deck -> Int
sum deck =
    AllDict.values deck
        |> List.sum


hash : Deck -> Maybe String
hash deck =
    toList deck
        |> Encode.hash


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



-- Encoder/Decoders


decoder : Value -> List ( String, Int )
decoder session =
    -- TODO: This is a frustrating data type to work with
    session
        |> decodeValue Decode.string
        |> Result.andThen (decodeString (Decode.keyValuePairs Decode.int))
        |> Result.withDefault []


encoder : Deck -> String
encoder deck =
    let
        toEncoder ( card, count ) =
            ( card.uid, Encode.int count )
    in
        List.map toEncoder (toList deck)
            |> Encode.object
            |> encode 0



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
