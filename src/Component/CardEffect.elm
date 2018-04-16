module Component.CardEffect
    exposing
        ( CardEffect
        , decoder
        , toHtml
        )

{-| This Component represents the effect of an individual card.


# Types

@docs CardEffect


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (div, text, span, Html)
import Html.Attributes exposing (class)
import Json.Decode exposing (string, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Data.CardSymbol as CardSymbol exposing (CardSymbol(..))


{-| A record containing the text and [Symbol](CardSymbol#CardSymbol) of a card.
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
    let
        image =
            CardSymbol.toHtml effect.symbol

        content =
            .text effect
    in
        div [ class "card-effect" ]
            [ image
            , span [ class "effect-text" ] [ text content ]
            ]
