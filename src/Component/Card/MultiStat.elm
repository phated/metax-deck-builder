module Component.Card.MultiStat
    exposing
        ( MultiStat
        , toHtml
        )

{-| Component.Card.MultiStat represents the stats of a Multi Battle Card.


# Types

@docs MultiStat


# Views

@docs toHtml

-}

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Component.Card.Rank as Rank exposing (Rank)
import Component.Card.StatType as StatType exposing (StatType)


{-| A container for a list of StatTypes and a Rank.
-}
type alias MultiStat =
    { statTypes : List StatType
    , rank : Rank
    }


{-| Renders the MultiStat as an Html view.
-}
toHtml : MultiStat -> Html msg
toHtml { statTypes, rank } =
    let
        -- TODO: This likely should be sorted
        statNames =
            List.map StatType.toString statTypes

        iconSrc =
            "/icons/" ++ (String.join "-" statNames) ++ ".png"
    in
        div [ class "card-stat" ]
            [ img [ class "card-stat-icon multi-stat-icon", src iconSrc ] []
            , Rank.toHtml rank
            ]
