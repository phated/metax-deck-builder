module Component.Deck.Battles.Multi
    exposing
        ( MultiBattles
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


type alias MultiBattles =
    Dict Rank (Dict Card Int)


toList : { a | multi : MultiBattles } -> List ( Card, Int )
toList deck =
    let
        lister rank dict list =
            List.append list (Dict.toList dict)
    in
        Dict.foldr lister [] deck.multi


foldr : (Rank -> Dict Card Int -> b -> b) -> b -> { a | multi : MultiBattles } -> b
foldr fn result deck =
    -- let
    --     subfold rank dict result =
    --         Dict.foldr fn result dict
    -- in
    Dict.foldr fn result deck.multi


insert : Rank -> ( Card, Int ) -> { a | multi : MultiBattles } -> { a | multi : MultiBattles }
insert rank ( card, count ) deck =
    let
        inserter maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.insert Card.order card count dict)

                Nothing ->
                    Just (Dict.singleton card count)

        multi =
            Dict.update byRank rank inserter deck.multi
    in
        { deck | multi = multi }


update : Rank -> ( Card, Maybe Int -> Maybe Int ) -> { a | multi : MultiBattles } -> { a | multi : MultiBattles }
update rank ( card, fn ) deck =
    let
        updater maybeDict =
            case maybeDict of
                Just dict ->
                    Just (Dict.update Card.order card fn dict)

                Nothing ->
                    Just (Dict.update Card.order card fn Dict.empty)

        multi =
            Dict.update byRank rank updater deck.multi
    in
        { deck | multi = multi }


count : Rank -> Card -> { a | multi : MultiBattles } -> Int
count rank card deck =
    Dict.get byRank rank deck.multi
        |> Maybe.map (Dict.get Card.order card >> Maybe.withDefault 0)
        |> Maybe.withDefault 0


sum : { a | multi : MultiBattles } -> Int
sum deck =
    let
        subfold rank dict result =
            Dict.foldr (\_ count result -> result + count) result dict
    in
        Dict.foldr subfold 0 deck.multi


byRank : Compare.Comparator Rank
byRank =
    Compare.by CardRank.order
