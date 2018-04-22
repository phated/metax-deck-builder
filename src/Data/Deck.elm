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
import Encode
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID


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
    Dict.fromList Card.order cards


foldl : (Card -> Int -> a -> a) -> a -> Deck -> a
foldl =
    Dict.foldl


count : Card -> Deck -> Int
count card deck =
    Dict.get Card.order card deck
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
    Dict.update Card.order card maybeIncrement deck


decrement : Card -> Deck -> Deck
decrement card deck =
    let
        updatedDeck =
            Dict.update Card.order card maybeDecrement deck
    in
        Dict.filter Card.order notZero updatedDeck



-- Encoder/Decoders


decoder : Decoder (List ( String, Int ))
decoder =
    -- TODO: This is a frustrating data type to work with
    Decode.keyValuePairs Decode.int


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
