module Component.Card.SingleStat
    exposing
        ( SingleStat
        , decoder
        , toHtml
        )

{-| Component.Card.SingleStat represents the stats of a Single Battle Card.


# Types

@docs SingleStat


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Component.Card.Rank as Rank exposing (Rank)
import Component.Card.StatType as StatType exposing (StatType)


{-| A container for a StatType and Rank.
-}
type alias SingleStat =
    { statType : StatType
    , rank : Rank
    }


{-| Decode a string into a SingleStat.
-}
decoder : Decoder SingleStat
decoder =
    decode SingleStat
        |> required "type" StatType.decoder
        |> required "rank" Rank.decoder


{-| Renders a SingleStat as an Html view.
-}
toHtml : SingleStat -> Html msg
toHtml { statType, rank } =
    div [ class "card-stat" ]
        [ StatType.toHtml statType
        , Rank.toHtml rank
        ]
