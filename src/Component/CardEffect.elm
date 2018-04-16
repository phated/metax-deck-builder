module Component.CardEffect
    exposing
        ( CardEffect
        , decoder
        , toHtml
        , toHtmlLazy
        )

{-| Component.CardEffect represents the effect of an individual card.


# Types

@docs CardEffect


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (div, text, span, Html)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (class)
import Json.Decode exposing (string, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Component.CardSymbol as CardSymbol exposing (CardSymbol)


{-| A record containing the text and [Symbol](CardSymbol#CardSymbol) of a card.

    CardEffect Play "Draw a card."

-}
type alias CardEffect =
    { symbol : CardSymbol
    , text : String
    }


{-| Decode a string into a CardEffect.
-}
decoder : Decoder CardEffect
decoder =
    decode CardEffect
        |> required "symbol" CardSymbol.decoder
        |> optional "text" string ""


{-| Render the CardEffect as an Html view.

    CardEffect.toHtml card.effect == div [ class "card-effect" ] [ ... ]

-}
toHtml : CardEffect -> Html msg
toHtml effect =
    div [ class "card-effect" ]
        [ CardSymbol.toHtmlLazy effect.symbol
        , span [ class "effect-text" ] [ text effect.text ]
        ]


{-| Render the CardEffect as a Lazy Html view to avoid re-rendering.
-}
toHtmlLazy : CardEffect -> Html msg
toHtmlLazy =
    lazy toHtml
