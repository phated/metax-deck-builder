module Data.Deck exposing (Deck, decoder, encoder, increment, decrement, empty, toList, fromList, count, sum, hash)

import AllDict exposing (AllDict)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Data.Card exposing (Card)
import Encode
import Compare exposing (concat, by, Comparator)
import Data.CardType as CardType exposing (CardType(Character, Event, Battle))
import Data.BattleType as BattleType


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
        |> sort
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



-- TODO: Dedupe the sorting


sort : List ( Card, Int ) -> List ( Card, Int )
sort cards =
    (List.sortWith order cards)


battleTypeOrder : Card -> Int
battleTypeOrder { card_type, stats } =
    case card_type of
        Battle ->
            BattleType.toInt stats

        Character ->
            0

        Event ->
            0


order : Comparator ( Card, Int )
order =
    concat
        [ by (CardType.toInt << .card_type << Tuple.first)
        , by (battleTypeOrder << Tuple.first)
        , by (Tuple.second)
        , by (.title << Tuple.first)
        , by (.text << .effect << Tuple.first)
        ]
