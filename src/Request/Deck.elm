module Request.Deck exposing (save, export)

import Json.Encode exposing (encode, string, int, list, object, Value)
import Data.Card exposing (Card)
import Data.Deck as Deck exposing (Deck)
import Data.CardType as CardType exposing (CardType)
import Ports


toEncoder : ( Card, Int ) -> ( String, Value )
toEncoder ( card, count ) =
    ( card.uid, int count )


save : Deck -> Cmd msg
save deck =
    List.map toEncoder (Deck.toList deck)
        |> object
        |> encode 0
        |> Just
        |> Ports.storeSession


toExport : ( Card, Int ) -> Value
toExport ( card, quantity ) =
    object
        [ ( "quantity", int quantity )
        , ( "id", string card.uid )
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
