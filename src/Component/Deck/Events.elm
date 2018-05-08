module Component.Deck.Events
    exposing
        ( Events
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


type alias Events =
    Dict Card Int


empty : Events
empty =
    Dict.empty


toList : { a | events : Events } -> List ( Card, Int )
toList deck =
    Dict.toList deck.events


foldr : (Card -> Int -> b -> b) -> b -> { a | events : Events } -> b
foldr fn result deck =
    Dict.foldr fn result deck.events


filter : (Card -> Int -> Bool) -> { a | events : Events } -> { a | events : Events }
filter fn deck =
    { deck | events = Dict.filter Card.order fn deck.events }


insert : Card -> Int -> { a | events : Events } -> { a | events : Events }
insert card count deck =
    { deck | events = Dict.insert Card.order card count deck.events }


update : Card -> (Maybe Int -> Maybe Int) -> { a | events : Events } -> { a | events : Events }
update card fn deck =
    { deck | events = Dict.update Card.order card fn deck.events }


count : Card -> { a | events : Events } -> Int
count card deck =
    Dict.get Card.order card deck.events
        |> Maybe.withDefault 0


sum : { a | events : Events } -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck.events