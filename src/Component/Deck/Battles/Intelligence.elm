module Component.Deck.Battles.Intelligence
    exposing
        ( IntelligenceBattles
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


type alias IntelligenceBattles =
    Dict Rank (Dict Card Int)


empty : IntelligenceBattles
empty =
    Dict.empty


toList : { a | intelligence : IntelligenceBattles } -> List ( Card, Int )
toList deck =
    let
        lister rank dict list =
            List.append list (Dict.toList dict)
    in
        Dict.foldr lister [] deck.intelligence


foldr : (Rank -> Dict Card Int -> b -> b) -> b -> { a | intelligence : IntelligenceBattles } -> b
foldr fn result deck =
    Dict.foldr fn result deck.intelligence


filter : (Card -> Int -> Bool) -> { a | intelligence : IntelligenceBattles } -> { a | intelligence : IntelligenceBattles }
filter fn deck =
    let
        subfilter rank dict result =
            Dict.insert byRank rank (Dict.filter Card.order fn dict) result
    in
        { deck | intelligence = Dict.foldr subfilter empty deck.intelligence }


insert : Rank -> ( Card, Int ) -> { a | intelligence : IntelligenceBattles } -> { a | intelligence : IntelligenceBattles }
insert rank ( card, count ) deck =
    let
        inserter maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.insert Card.order card count dict)

                Nothing ->
                    Just (Dict.singleton card count)

        intelligence =
            Dict.update byRank rank inserter deck.intelligence
    in
        { deck | intelligence = intelligence }


update : Rank -> ( Card, Maybe Int -> Maybe Int ) -> { a | intelligence : IntelligenceBattles } -> { a | intelligence : IntelligenceBattles }
update rank ( card, fn ) deck =
    let
        updater maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.update Card.order card fn dict)

                Nothing ->
                    Just (Dict.update Card.order card fn Dict.empty)

        intelligence =
            Dict.update byRank rank updater deck.intelligence
    in
        { deck | intelligence = intelligence }


count : Rank -> Card -> { a | intelligence : IntelligenceBattles } -> Int
count rank card deck =
    Dict.get byRank rank deck.intelligence
        |> Maybe.map (Dict.get Card.order card >> Maybe.withDefault 0)
        |> Maybe.withDefault 0


sum : { a | intelligence : IntelligenceBattles } -> Int
sum deck =
    let
        subfold rank dict result =
            Dict.foldr (\_ count result -> result + count) result dict
    in
        Dict.foldr subfold 0 deck.intelligence


byRank : Compare.Comparator Rank
byRank =
    Compare.by CardRank.order
