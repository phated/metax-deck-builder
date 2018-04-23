module Component.Card.Stats
    exposing
        ( Stats
        , decoder
        , toHtml
        , foldr
        )

{-| Component.Card.Stats represents all stats of a card.


# Types

@docs Stats


# Build


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (Html, div, text, img, span)
import Html.Attributes exposing (class, src)
import Json.Decode exposing (Decoder, list, field, map, map2, at, succeed, fail, andThen, string, int)
import Json.Decode.Pipeline exposing (decode, required, optional, resolve)
import Component.Card.Stat as CardStat exposing (Stat)
import Component.Card.Type as CardType exposing (Type(Character, Event, Battle))
import Component.Card.StatType as StatType
import Util exposing (decoderFromMaybe)


{-| The stats of a card.
-}
type Stats
    = Many (List Stat)
    | One Stat
    | None


{-| Decodes a string into a StatList
-}
decoder : Decoder Stats
decoder =
    decode toStats
        |> required "type" CardType.decoder
        |> optional "stats" (list CardStat.decoder) []
        |> resolve


{-| Renders a StatList to a list of Html views.

TODO: This return signature messes up lazy rendering

-}
toHtml : Stats -> List (Html msg)
toHtml s =
    case s of
        Many stats ->
            List.map CardStat.toHtml stats

        One stat ->
            List.singleton (CardStat.toHtml stat)

        None ->
            []


foldr : (Stat -> a -> a) -> a -> Stats -> a
foldr func acc s =
    -- TODO: eventually remove?
    let
        statList =
            case s of
                Many stats ->
                    stats

                One stat ->
                    [ stat ]

                None ->
                    []
    in
        List.foldr func acc statList



{- Internals -}


toStats : Type -> List Stat -> Decoder Stats
toStats cardType stats =
    -- TODO: Maybe there should be more validation around duplicate StatType in the list
    case cardType of
        Character ->
            succeed (Many stats)

        Event ->
            succeed None

        Battle ->
            collapse stats |> decoderFromMaybe "Invalid stats for a battle card."


collapse : List Stat -> Maybe Stats
collapse stats =
    case stats of
        [ s1 ] ->
            Just <| One s1

        [ s1, s2 ] ->
            Just <|
                One
                    { rank = s1.rank
                    , stat_type =
                        StatType.collapse
                            [ s1.stat_type
                            , s2.stat_type
                            ]
                    }

        [ s1, s2, s3 ] ->
            Just <|
                One
                    { rank = s1.rank
                    , stat_type =
                        StatType.Multi
                            [ s1.stat_type
                            , s2.stat_type
                            , s3.stat_type
                            ]
                    }

        _ ->
            Nothing
