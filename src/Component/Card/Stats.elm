module Component.Card.Stats
    exposing
        ( Stats(..)
        , decoder
        , toHtml
        , toBattleRank
        , order
        )

{-| Component.Card.Stats represents the different types of stats a card could have.
Including a list of stats (Characters), a single stat type + rank (normal Battle cards),
multiple stat types but one rank (multi Battle cards), or none (Events)


# Types

@docs Stats


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml, toBattleRank


# Ordering

@docs order

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode exposing (Decoder, list, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Component.Card.Type as CardType exposing (Type(Character, Event, Battle))
import Component.Card.Rank as Rank exposing (Rank)
import Component.Card.StatType as StatType exposing (StatType)
import Component.Card.SingleStat as SingleStat exposing (SingleStat)
import Component.Card.MultiStat as MultiStat exposing (MultiStat)


{-| The stats on a card.
-}
type Stats
    = StatList (List SingleStat)
    | Single SingleStat
    | Multi MultiStat
    | None


{-| Decode a string into Stats.
-}
decoder : Decoder Stats
decoder =
    decode toStats
        |> required "type" CardType.decoder
        |> required "stats" (list SingleStat.decoder)
        |> resolve


{-| Renders Stats as an Html view.
-}
toHtml : Stats -> Html msg
toHtml stats =
    let
        children =
            case stats of
                Single stat ->
                    List.singleton (SingleStat.toHtml stat)

                Multi stat ->
                    List.singleton (MultiStat.toHtml stat)

                StatList stats ->
                    List.map SingleStat.toHtml stats

                _ ->
                    []
    in
        -- TODO: This needs better CSS and less wrappers
        div [ class "card-stats" ] children


{-| Pulls a Rank from the Stats only if they are for a Battle Card.
-}
toBattleRank : Stats -> Maybe Rank
toBattleRank stats =
    -- TODO: I want this to be gone
    case stats of
        StatList _ ->
            Nothing

        Single stat ->
            Just stat.rank

        Multi stat ->
            Just stat.rank

        None ->
            Nothing


{-| Convert Stats into a comparable. This is useful when used with `Compare.by`.
-}
order : Stats -> Int
order stats =
    case stats of
        Single { statType, rank } ->
            (StatType.order statType) + (Rank.order rank)

        Multi { rank } ->
            21 + (Rank.order rank)

        StatList _ ->
            -- TODO: Should the StatList effect the sort order?
            0

        None ->
            0



-- Internals


toStats : Type -> List SingleStat -> Decoder Stats
toStats cardType stats =
    case cardType of
        Character ->
            succeed (StatList stats)

        Event ->
            case stats of
                [] ->
                    succeed None

                _ ->
                    fail "An Event card should not have stats."

        Battle ->
            -- TODO: Maybe replace the ofValue* methods with https://github.com/elm-community/list-extra/blob/7.1.0/src/List/Extra.elm#L973
            case stats of
                [ statOne, statTwo, statThree ] ->
                    let
                        statTypes =
                            [ statOne.statType, statTwo.statType, statThree.statType ]

                        maybeRank =
                            Rank.ofValue3 statOne.rank statTwo.rank statThree.rank
                    in
                        case maybeRank of
                            Just rank ->
                                succeed (Multi { statTypes = statTypes, rank = rank })

                            Nothing ->
                                fail "Invalid rank for Multi3 Battle Card."

                [ statOne, statTwo ] ->
                    let
                        statTypes =
                            [ statOne.statType, statTwo.statType ]

                        maybeRank =
                            Rank.ofValue2 statOne.rank statTwo.rank
                    in
                        case maybeRank of
                            Just rank ->
                                succeed (Multi { statTypes = statTypes, rank = rank })

                            Nothing ->
                                fail "Invalid rank for Multi2 Battle Card."

                [ statOne ] ->
                    let
                        statType =
                            statOne.statType

                        maybeRank =
                            Rank.ofValue statOne.rank
                    in
                        case maybeRank of
                            Just rank ->
                                succeed (Single { statType = statType, rank = rank })

                            Nothing ->
                                fail "Invalid rank for Single stat Battle Card."

                _ ->
                    fail "Invalid card stats."
