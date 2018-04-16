module Component.Card.Stat
    exposing
        ( Stat(..)
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


{-| A stat on a card. The stat type wraps the rank.
-}
type Stat
    = Strength Int
    | Intelligence Int
    | Special Int


{-| Decode a string into a Stat.
-}
decoder : Decoder Stat
decoder =
    -- TODO: Is there a better way to do this?
    decode toStat
        |> required "type" string
        |> required "rank" int
        |> resolve


{-| Render the Stat as an Html view.
-}
toHtml : Stat -> Html msg
toHtml stat =
    let
        ( imgSrc, rank ) =
            case stat of
                Strength rank ->
                    ( "/icons/strength.png", rank )

                Intelligence rank ->
                    ( "/icons/intelligence.png", rank )

                Special rank ->
                    ( "/icons/special.png", rank )
    in
        div [ class "card-stat" ]
            [ img [ class "card-stat-icon", src imgSrc ] []
            , span [ class "card-stat-text" ] [ text (toString rank) ]
            ]


{-| Render the Stat as a Lazy Html view to avoid re-rendering.
-}
toHtmlLazy : Stat -> Html msg
toHtmlLazy =
    lazy toHtml


{-| A non-exported helper to convert from an object to a union type.
-}
toStat : String -> Int -> Decoder Stat
toStat statType rank =
    case statType of
        "Strength" ->
            succeed (Strength rank)

        "Intelligence" ->
            succeed (Intelligence rank)

        "Special" ->
            succeed (Special rank)

        _ ->
            fail "Invalid card stat."
