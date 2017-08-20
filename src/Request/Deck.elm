module Request.Deck exposing (save, export)

import Json.Encode exposing (encode, string, int, list, object, Value)
import Json.Helpers exposing (encodeMap)
import Data.Deck exposing (Deck)
import Data.Card exposing (Card)
import Data.CardType exposing (CardType(Unknown), fromCardType)
import Data.CardList exposing (CardList)
import Dict
import Ports


save : Deck -> Cmd msg
save deck =
    encodeMap string int deck
        |> encode 0
        |> Just
        |> Ports.storeSession


toExport : CardList -> ( String, Int ) -> Value
toExport cards ( cardId, quantity ) =
    let
        title =
            lookupTitle cards cardId

        cardType =
            lookupType cards cardId
    in
        object
            [ ( "quantity", int quantity )
            , ( "id", string cardId )
            , ( "title", string title )
            , ( "card_type", string (fromCardType cardType) )
            ]


export : CardList -> Deck -> Cmd msg
export cards deck =
    let
        exportDeck =
            List.map (toExport cards) (Dict.toList deck)
    in
        encode 0 (list exportDeck)
            |> Just
            |> Ports.exportSession



-- TODO: dedupe


idMatches : String -> Card -> Bool
idMatches cardId card =
    card.id == cardId


lookup : CardList -> String -> Maybe Card
lookup cards cardId =
    List.filter (idMatches cardId) cards
        |> List.head


lookupTitle : CardList -> String -> String
lookupTitle cards cardId =
    let
        card =
            lookup cards cardId
    in
        case card of
            Just card ->
                .title card

            _ ->
                ""


lookupType : CardList -> String -> CardType
lookupType cards cardId =
    let
        card =
            lookup cards cardId
    in
        case card of
            Just card ->
                .card_type card

            _ ->
                Unknown
