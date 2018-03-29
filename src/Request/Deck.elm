module Request.Deck exposing (save, export)

import Json.Encode exposing (encode, string, int, list, object, Value)
import Json.Helpers exposing (encodeMap)
import Data.Deck exposing (Deck)
import Data.Card exposing (Card)
import Data.CardType as CardType exposing (CardType)
import Data.CardList exposing (CardList)
import Dict
import Ports


save : Deck -> Cmd msg
save deck =
    encodeMap string int deck
        |> encode 0
        |> Just
        |> Ports.storeSession


toExport : CardList -> ( String, Int ) -> Maybe Value
toExport cards ( cardId, quantity ) =
    let
        card =
            lookup cards cardId
    in
        case card of
            Just card ->
                Just <|
                    object
                        [ ( "quantity", int quantity )
                        , ( "id", string cardId )
                        , ( "title", string <| .title card )
                        , ( "card_type", string (CardType.toString <| .card_type card) )
                        ]

            Nothing ->
                Nothing


export : CardList -> Deck -> Cmd msg
export cards deck =
    let
        exportDeck =
            List.filterMap (toExport cards) (Dict.toList deck)
    in
        encode 0 (list exportDeck)
            |> Just
            |> Ports.exportSession



-- TODO: dedupe


idMatches : String -> Card -> Bool
idMatches cardId card =
    card.uid == cardId


lookup : CardList -> String -> Maybe Card
lookup cards cardId =
    List.filter (idMatches cardId) cards
        |> List.head
