module Component.Deck.Battles.Strength
    exposing
        ( foldr
          -- , filter
          -- , insert
          -- , update
          -- , count
        , sum
        )

import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.Rank as CardRank exposing (Rank)
import Component.Card.Stats exposing (Stats(Single))
import Component.Card.StatType as StatType exposing (StatType(Strength))
import Compare
import Component.Deck.Battles as Battles exposing (Battles)


foldr : (Card -> Int -> b -> b) -> b -> { a | battles : Battles } -> b
foldr fn result deck =
    let
        onlyStrength card count result =
            case card.stats of
                Single { statType, rank } ->
                    case statType of
                        Strength ->
                            fn card count result

                        _ ->
                            result

                _ ->
                    result
    in
        Dict.foldr fn result deck.battles



-- filter : (Card -> Int -> Bool) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
-- filter fn deck =
--     let
--         subfilter rank dict result =
--             Dict.insert byRank rank (Dict.filter Card.order fn dict) result
--     in
--         { deck | strength = Dict.foldr subfilter empty deck.strength }
-- insert : Rank -> ( Card, Int ) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
-- insert rank ( card, count ) deck =
--     let
--         inserter maybeDict =
--             case maybeDict of
--                 Just dict ->
--                     Just (Dict.insert Card.order card count dict)
--                 Nothing ->
--                     Just (Dict.singleton card count)
--         strength =
--             Dict.update byRank rank inserter deck.strength
--     in
--         { deck | strength = strength }
-- update : Rank -> ( Card, Maybe Int -> Maybe Int ) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
-- update rank ( card, fn ) deck =
--     let
--         updater maybeDict =
--             case maybeDict of
--                 Just dict ->
--                     Just (Dict.update Card.order card fn dict)
--                 Nothing ->
--                     Just (Dict.update Card.order card fn Dict.empty)
--         strength =
--             Dict.update byRank rank updater deck.strength
--     in
--         { deck | strength = strength }
-- count : Card -> { a | battles : Battles } -> Int
-- count card deck =
--     Dict.get Card.order card deck.strength
--         |> Maybe.withDefault 0


sum : { a | battles : Battles } -> Int
sum deck =
    foldr (\_ count result -> result + count) 0 deck



-- toHtml : { a | strength : StrengthBattles } -> Html msg
-- toHtml deck =
--     div [ class "subheader-title" ]
--         [ text <| title ++ " (" ++ countString ++ ")" ]
--         (battleCardSubSection (title ++ " - Rank " ++ (Rank.toString rank)) cards)
-- Internals
-- byRank : Compare.Comparator Rank
-- byRank =
--     Compare.by CardRank.order
