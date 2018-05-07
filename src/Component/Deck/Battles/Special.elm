module Component.Deck.Battles.Special
    exposing
        ( SpecialBattles
        , insert
        , update
        , count
        , sum
        , toList
        , foldr
        )

import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.Rank as CardRank exposing (Rank)
import Compare


type alias SpecialBattles =
    Dict Rank (Dict Card Int)


toList : { a | special : SpecialBattles } -> List ( Card, Int )
toList deck =
    let
        lister rank dict list =
            List.append list (Dict.toList dict)
    in
        Dict.foldr lister [] deck.special


foldr : (Rank -> Dict Card Int -> b -> b) -> b -> { a | special : SpecialBattles } -> b
foldr fn result deck =
    -- let
    --     subfold rank dict result =
    --         Dict.foldr fn result dict
    -- in
    Dict.foldr fn result deck.special


insert : Rank -> ( Card, Int ) -> { a | special : SpecialBattles } -> { a | special : SpecialBattles }
insert rank ( card, count ) deck =
    let
        inserter maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.insert Card.order card count dict)

                Nothing ->
                    Just (Dict.singleton card count)

        special =
            Dict.update byRank rank inserter deck.special
    in
        { deck | special = special }


update : Rank -> ( Card, Maybe Int -> Maybe Int ) -> { a | special : SpecialBattles } -> { a | special : SpecialBattles }
update rank ( card, fn ) deck =
    let
        updater maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.update Card.order card fn dict)

                Nothing ->
                    Just (Dict.update Card.order card fn Dict.empty)

        special =
            Dict.update byRank rank updater deck.special
    in
        { deck | special = special }


count : Rank -> Card -> { a | special : SpecialBattles } -> Int
count rank card deck =
    Dict.get byRank rank deck.special
        |> Maybe.map (Dict.get Card.order card >> Maybe.withDefault 0)
        |> Maybe.withDefault 0


sum : { a | special : SpecialBattles } -> Int
sum deck =
    let
        subfold rank dict result =
            Dict.foldr (\_ count result -> result + count) result dict
    in
        Dict.foldr subfold 0 deck.special


byRank : Compare.Comparator Rank
byRank =
    Compare.by CardRank.order
