module Component.Deck.Battles
    exposing
        ( Battles
        , empty
        , foldr
        , filter
        , insert
        , update
        , count
        , sum
        , toList
        , toHtml
        )

import Html exposing (Html, div)
import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.Stats exposing (Stats(Single, Multi))
import Component.Card.StatType exposing (StatType(Strength, Intelligence, Special))


-- import Component.Deck.Battles.Strength as StrengthBattles exposing (StrengthBattles)
-- import Component.Deck.Battles.Intelligence as IntelligenceBattles exposing (IntelligenceBattles)
-- import Component.Deck.Battles.Special as SpecialBattles exposing (SpecialBattles)
-- import Component.Deck.Battles.Multi as MultiBattles exposing (MultiBattles)


type alias Battles =
    Dict Card Int


empty : Battles
empty =
    Dict.empty


toList : { a | battles : Battles } -> List ( Card, Int )
toList deck =
    Dict.toList deck.battles


foldr : (Card -> Int -> b -> b) -> b -> { a | battles : Battles } -> b
foldr fn result deck =
    Dict.foldr fn result deck.battles


filter : (Card -> Int -> Bool) -> { a | battles : Battles } -> { a | battles : Battles }
filter fn deck =
    { deck | battles = Dict.filter Card.order fn deck.battles }


map : (Card -> Int -> b) -> { a | battles : Battles } -> List b
map fn deck =
    let
        append card count result =
            (fn card count) :: result
    in
        foldr append [] deck


insert : Card -> Int -> { a | battles : Battles } -> { a | battles : Battles }
insert card count deck =
    { deck | battles = Dict.insert Card.order card count deck.battles }


update : Card -> (Maybe Int -> Maybe Int) -> { a | battles : Battles } -> { a | battles : Battles }
update card fn deck =
    { deck | battles = Dict.update Card.order card fn deck.battles }


count : Card -> { a | battles : Battles } -> Int
count card deck =
    Dict.get Card.order card deck.battles
        |> Maybe.withDefault 0


sum : { a | battles : Battles } -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck.battles


toHtml : { a | battles : Battles } -> Html msg
toHtml deck =
    div [] []
