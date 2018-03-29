module Data.CardEffect exposing (CardEffect, decoder, toHtml)

import Html exposing (text, span, Html)
import Html.Attributes exposing (class)
import Json.Decode exposing (string, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Data.CardSymbol as CardSymbol exposing (CardSymbol(..))


type alias CardEffect =
    { symbol : CardSymbol
    , text : String
    }


decoder : Decoder CardEffect
decoder =
    decode CardEffect
        |> required "symbol" CardSymbol.decoder
        |> optional "text" string ""


toHtml : CardEffect -> List (Html msg)
toHtml effect =
    let
        image =
            CardSymbol.toHtml effect.symbol

        content =
            .text effect
    in
        [ image
        , span [ class "effect-text" ] [ text content ]
        ]
