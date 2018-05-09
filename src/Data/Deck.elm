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
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Encode
import Component.Card exposing (Card)
import Component.Card.UID as CardUID
import Component.Card.Type exposing (Type(Character, Event, Battle))
import Component.Deck.Events as Events exposing (Events)
import Component.Deck.Battles as Battles exposing (Battles)
import Component.Deck.Characters as Characters exposing (Characters)


-- import Component.Deck.Battles.Strength as StrengthBattles exposing (StrengthBattles)
-- import Component.Deck.Battles.Intelligence as IntelligenceBattles exposing (IntelligenceBattles)
-- import Component.Deck.Battles.Special as SpecialBattles exposing (SpecialBattles)
-- import Component.Deck.Battles.Multi as MultiBattles exposing (MultiBattles)


type alias Deck =
    { characters : Characters
    , events : Events
    , battles : Battles

    -- , strength : StrengthBattles
    -- , intelligence : IntelligenceBattles
    -- , special : SpecialBattles
    -- , multi : MultiBattles
    }


empty : Deck
empty =
    { characters = Characters.empty
    , events = Events.empty
    , battles = Battles.empty

    -- , strength = StrengthBattles.empty
    -- , intelligence = IntelligenceBattles.empty
    -- , special = SpecialBattles.empty
    -- , multi = MultiBattles.empty
    }


toList : Deck -> List ( Card, Int )
toList deck =
    List.concat
        [ Characters.toList deck
        , Events.toList deck
        , Battles.toList deck
        ]


fromList : List ( Card, Int ) -> Deck
fromList cards =
    List.foldr (uncurry insert) empty cards


insert : Card -> Int -> Deck -> Deck
insert card count deck =
    case card.card_type of
        Character ->
            Characters.insert card count deck

        Event ->
            Events.insert card count deck

        Battle ->
            Battles.insert card count deck


update : Card -> (Maybe Int -> Maybe Int) -> Deck -> Deck
update card fn deck =
    case card.card_type of
        Character ->
            Characters.update card fn deck

        Event ->
            Events.update card fn deck

        Battle ->
            Battles.update card fn deck


foldr : (Card -> Int -> a -> a) -> a -> Deck -> a
foldr fn result deck =
    let
        logFn =
            Debug.log "foldr" >> fn
    in
        Characters.foldr fn result deck
            |> flip (Events.foldr fn) deck
            |> flip (Battles.foldr logFn) deck


filter : (Card -> Int -> Bool) -> Deck -> Deck
filter fn deck =
    deck
        |> Characters.filter fn
        |> Events.filter fn
        |> Battles.filter fn


count : Card -> Deck -> Int
count card deck =
    case card.card_type of
        Character ->
            Characters.count card deck

        Event ->
            Events.count card deck

        Battle ->
            Battles.count card deck


sum : Deck -> Int
sum deck =
    (Characters.sum deck) + (Events.sum deck) + (Battles.sum deck)


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
            foldr Encode.encodeCard result deck

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
    update card maybeIncrement deck


decrement : Card -> Deck -> Deck
decrement card deck =
    let
        updatedDeck =
            update card maybeDecrement deck
    in
        filter notZero updatedDeck



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


toHtml : Deck -> Html msg
toHtml deck =
    div [ id "deck-list-pane", class "pane" ]
        [ Characters.toHtml deck
        , Events.toHtml deck
        , Battles.toHtml deck
        ]



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
