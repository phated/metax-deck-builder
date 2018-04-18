module Request.Deck exposing (save, export)

import Json.Encode exposing (encode, string, int, list, object, Value)
import Data.Deck as Deck exposing (Deck)
import Ports
import Component.Card exposing (Card)
import Component.Card.UID as CardUID
import Component.Card.Type as CardType


save : Deck -> Cmd msg
save deck =
    Deck.encoder deck
        |> Just
        |> Ports.storeSession


toExport : ( Card, Int ) -> Value
toExport ( card, quantity ) =
    object
        [ ( "quantity", int quantity )
        , ( "id", string (CardUID.toString card.uid) )
        , ( "title", string <| .title card )
        , ( "card_type", string (CardType.toString <| .card_type card) )
        ]


export : Deck -> Cmd msg
export deck =
    let
        exportDeck =
            List.map toExport (Deck.toList deck)
    in
        encode 0 (list exportDeck)
            |> Just
            |> Ports.exportSession
