module Component.Card.Effect
    exposing
        ( Effect
        , decoder
        , toHtml
        , toHtmlLazy
        )

{-| Component.Card.Effect represents the effect of an individual card.


# Types

@docs Effect


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
import Component.Card.Symbol as CardSymbol exposing (Symbol)


{-| A record containing the text and Symbol of a card.

    Component.Card.Effect Play "Draw a card."

-}
type alias Effect =
    { symbol : Symbol
    , text : String
    }


{-| Decode a string into a Effect.
-}
decoder : Decoder Effect
decoder =
    decode Effect
        |> required "symbol" CardSymbol.decoder
        |> optional "text" string ""


{-| Render the Effect as an Html view.

    Component.Card.Effect.toHtml card.effect == div [ class "card-effect" ] [ ... ]

-}
toHtml : Effect -> Html msg
toHtml effect =
    div [ class "card-effect" ]
        [ CardSymbol.toHtmlLazy effect.symbol
        , span [ class "effect-text" ] [ text effect.text ]
        ]


{-| Render the Effect as a Lazy Html view to avoid re-rendering.
-}
toHtmlLazy : Effect -> Html msg
toHtmlLazy =
    lazy toHtml
