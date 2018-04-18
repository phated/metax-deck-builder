module Data.Deck
    exposing
        ( Deck
        , decoder
        , encoder
        , increment
        , decrement
        , empty
        , toList
        , fromList
        , count
        , sum
        , hash
        , foldl
        )

import Avl.Dict as Dict exposing (Dict)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Data.Card exposing (Card)
import Data.CardUID as CardUID
import Encode
import Compare exposing (concat, by, Comparator)
import Data.CardType as CardType exposing (CardType(Character, Event, Battle))
import Data.BattleType as BattleType


type alias Deck =
    Dict Card Int


empty : Deck
empty =
    Dict.empty


toList : Deck -> List ( Card, Int )
toList deck =
    Dict.toList deck


fromList : List ( Card, Int ) -> Deck
fromList cards =
    Dict.fromList order cards


foldl : (Card -> Int -> a -> a) -> a -> Deck -> a
foldl =
    Dict.foldl


count : Card -> Deck -> Int
count card deck =
    Dict.get order card deck
        |> Maybe.withDefault 0


sum : Deck -> Int
sum deck =
    Dict.foldl (\_ count result -> result + count) 0 deck


hash : Deck -> Maybe String
hash deck =
    let
        version =
            0

        encodedVersion =
            Encode.toBase64 version

        result =
            { cardHashes = []
            , checksum = Encode.encodeChecksum encodedVersion
            }

        encodeResult =
            Dict.foldr Encode.encodeCard result deck

        encodedDeck =
            encodedVersion ++ String.join "" encodeResult.cardHashes

        base64Checksum =
            Encode.toBase64 <| encodeResult.checksum % 64

        encoded =
            encodedDeck ++ base64Checksum
    in
        -- AiAASAhA
        if List.length encodeResult.cardHashes > 0 then
            Just encoded
        else
            Nothing


increment : Card -> Deck -> Deck
increment card deck =
    Dict.update order card maybeIncrement deck


decrement : Card -> Deck -> Deck
decrement card deck =
    let
        updatedDeck =
            Dict.update order card maybeDecrement deck
    in
        Dict.filter order notZero updatedDeck



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
            ( CardUID.toString card.uid, Encode.int count )
    in
        List.map toEncoder (toList deck)
            |> Encode.object
            |> encode 0



-- Utils


toComparable : Card -> String
toComparable card =
    CardUID.toString card.uid


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



-- TODO: Dedupe the sorting


battleTypeOrder : Card -> Int
battleTypeOrder { card_type, stats } =
    case card_type of
        Battle ->
            BattleType.toInt stats

        Character ->
            0

        Event ->
            0


order : Comparator Card
order =
    concat
        [ by (CardType.toInt << .card_type)
        , by (battleTypeOrder)
        , by (.title)
        , by (.text << .effect)
        ]
