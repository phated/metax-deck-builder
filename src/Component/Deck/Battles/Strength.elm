module Component.Deck.Battles.Strength
    exposing
        ( StrengthBattles
        , empty
        , foldr
        , filter
        , insert
        , update
        , count
        , sum
        , toList
        )

import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.Rank as CardRank exposing (Rank)
import Compare


type alias StrengthBattles =
    Dict Rank (Dict Card Int)


empty : StrengthBattles
empty =
    Dict.empty


toList : { a | strength : StrengthBattles } -> List ( Card, Int )
toList deck =
    let
        lister rank dict list =
            List.append list (Dict.toList dict)
    in
        Dict.foldr lister [] deck.strength


foldr : (Rank -> Dict Card Int -> b -> b) -> b -> { a | strength : StrengthBattles } -> b
foldr fn result deck =
    Dict.foldr fn result deck.strength


filter : (Card -> Int -> Bool) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
filter fn deck =
    let
        subfilter rank dict result =
            Dict.insert byRank rank (Dict.filter Card.order fn dict) result
    in
        { deck | strength = Dict.foldr subfilter empty deck.strength }


insert : Rank -> ( Card, Int ) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
insert rank ( card, count ) deck =
    let
        inserter maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.insert Card.order card count dict)

                Nothing ->
                    Just (Dict.singleton card count)

        strength =
            Dict.update byRank rank inserter deck.strength
    in
        { deck | strength = strength }


update : Rank -> ( Card, Maybe Int -> Maybe Int ) -> { a | strength : StrengthBattles } -> { a | strength : StrengthBattles }
update rank ( card, fn ) deck =
    let
        updater maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.update Card.order card fn dict)

                Nothing ->
                    Just (Dict.update Card.order card fn Dict.empty)

        strength =
            Dict.update byRank rank updater deck.strength
    in
        { deck | strength = strength }


count : Rank -> Card -> { a | strength : StrengthBattles } -> Int
count rank card deck =
    Dict.get byRank rank deck.strength
        |> Maybe.map (Dict.get Card.order card >> Maybe.withDefault 0)
        |> Maybe.withDefault 0


sum : { a | strength : StrengthBattles } -> Int
sum deck =
    let
        subfold rank dict result =
            Dict.foldr (\_ count result -> result + count) result dict
    in
        Dict.foldr subfold 0 deck.strength


byRank : Compare.Comparator Rank
byRank =
    Compare.by CardRank.order
