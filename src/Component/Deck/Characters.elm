module Component.Deck.Characters
    exposing
        ( Characters
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


type alias Characters =
    Dict Card Int


empty : Characters
empty =
    Dict.empty


toList : { a | characters : Characters } -> List ( Card, Int )
toList deck =
    Dict.toList deck.characters


foldr : (Card -> Int -> b -> b) -> b -> { a | characters : Characters } -> b
foldr fn result deck =
    Dict.foldr fn result deck.characters


filter : (Card -> Int -> Bool) -> { a | characters : Characters } -> { a | characters : Characters }
filter fn deck =
    { deck | characters = Dict.filter Card.order fn deck.characters }


insert : Card -> Int -> { a | characters : Characters } -> { a | characters : Characters }
insert card count deck =
    { deck | characters = Dict.insert Card.order card count deck.characters }


update : Card -> (Maybe Int -> Maybe Int) -> { a | characters : Characters } -> { a | characters : Characters }
update card fn deck =
    { deck | characters = Dict.update Card.order card fn deck.characters }


count : Card -> { a | characters : Characters } -> Int
count card deck =
    Dict.get Card.order card deck.characters
        |> Maybe.withDefault 0


sum : { a | characters : Characters } -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck.characters
