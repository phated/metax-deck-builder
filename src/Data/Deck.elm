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
        , foldr
        )

import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Encode
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID
import Component.Card.Type as CardType exposing (Type(Character, Event, Battle))
import Component.Deck.CardList as DeckCardList exposing (CardList)


type Deck
    = Deck
        { characters : CardList
        , events : CardList
        , battles : CardList
        }


empty : Deck
empty =
    Deck
        { characters = DeckCardList.empty
        , events = DeckCardList.empty
        , battles = DeckCardList.empty
        }


toList : Deck -> List ( Card, Int )
toList (Deck deck) =
    List.concat
        [ DeckCardList.toList deck.characters
        , DeckCardList.toList deck.events
        , DeckCardList.toList deck.battles
        ]


fromList : List ( Card, Int ) -> Deck
fromList cards =
    let
        insertTuple ( card, count ) =
            insert card count
    in
        List.foldr insertTuple empty cards


insert : Card -> Int -> Deck -> Deck
insert card count (Deck deck) =
    case card.card_type of
        Character ->
            Deck { deck | characters = DeckCardList.insert card count deck.characters }

        Event ->
            Deck { deck | events = DeckCardList.insert card count deck.events }

        Battle ->
            Deck { deck | battles = DeckCardList.insert card count deck.battles }


foldl : (Card -> Int -> a -> a) -> a -> Deck -> a
foldl func acc1 (Deck deck) =
    -- TODO: this should be in reverse?
    let
        acc2 =
            DeckCardList.foldl func acc1 deck.characters

        acc3 =
            DeckCardList.foldl func acc2 deck.events

        acc4 =
            DeckCardList.foldl func acc3 deck.battles
    in
        acc4


foldr : (Card -> Int -> a -> a) -> a -> Deck -> a
foldr func acc1 (Deck deck) =
    let
        acc2 =
            DeckCardList.foldr func acc1 deck.characters

        acc3 =
            DeckCardList.foldr func acc2 deck.events

        acc4 =
            DeckCardList.foldr func acc3 deck.battles
    in
        acc4


count : Card -> Deck -> Int
count card (Deck deck) =
    case card.card_type of
        Character ->
            DeckCardList.count card deck.characters

        Event ->
            DeckCardList.count card deck.events

        Battle ->
            DeckCardList.count card deck.battles


sum : Deck -> Int
sum (Deck deck) =
    DeckCardList.sum deck.characters + DeckCardList.sum deck.events + DeckCardList.sum deck.battles


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
increment card (Deck deck) =
    case card.card_type of
        Character ->
            Deck { deck | characters = DeckCardList.increment card deck.characters }

        Event ->
            Deck { deck | events = DeckCardList.increment card deck.events }

        Battle ->
            Deck { deck | battles = DeckCardList.increment card deck.battles }


decrement : Card -> Deck -> Deck
decrement card (Deck deck) =
    case card.card_type of
        Character ->
            Deck { deck | characters = DeckCardList.decrement card deck.characters }

        Event ->
            Deck { deck | events = DeckCardList.decrement card deck.events }

        Battle ->
            Deck { deck | battles = DeckCardList.decrement card deck.battles }



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
toHtml (Deck deck) =
    div [ id "deck-list-pane", class "pane" ]
        -- (List.concat
        [ DeckCardList.toHtml deck.characters
        , DeckCardList.toHtml deck.events
        , DeckCardList.toHtml deck.battles

        -- )
        ]
