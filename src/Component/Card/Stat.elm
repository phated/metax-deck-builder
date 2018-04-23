module Component.Card.Stat
    exposing
        ( Stat
        , decoder
        , toHtml
        , toHtmlLazy
        )

{-| Component.Card.Stat represents one stat on a card.


# Types

@docs Stat


# Build


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, div, img, span, text)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (class, src)
import Json.Decode exposing (int, string, succeed, fail, Decoder)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Component.Card.StatType as CardStatType exposing (StatType)


{-| A stat on a card. Contains the StatType and Rank
-}
type alias Stat =
    { stat_type : StatType
    , rank : Int
    }


{-| Decode a string into a Stat.
-}
decoder : Decoder Stat
decoder =
    decode Stat
        |> required "type" CardStatType.decoder
        |> required "rank" int


{-| Render the Stat as an Html view.
-}
toHtml : Stat -> Html msg
toHtml stat =
    div [ class "card-stat" ]
        [ img [ class "card-stat-icon", src (CardStatType.toImgUrl stat.stat_type) ] []
        , span [ class "card-stat-text" ] [ text (toString stat.rank) ]
        ]


{-| Render the Stat as a Lazy Html view to avoid re-rendering.
-}
toHtmlLazy : Stat -> Html msg
toHtmlLazy =
    lazy toHtml
