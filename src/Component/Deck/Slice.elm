module Component.Deck.Slice
    exposing
        ( DeckSlice
        , empty
        , size
        , isEmpty
        , foldr
        , filter
        , map
        , insert
        , update
        , count
        , sum
        , toList
        , toHtml
        )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Helpers
import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)


type alias DeckSlice =
    Dict Card Int


empty : DeckSlice
empty =
    Dict.empty


size : DeckSlice -> Int
size deck =
    Dict.size deck


isEmpty : DeckSlice -> Bool
isEmpty deck =
    size deck == 0


toList : DeckSlice -> List ( Card, Int )
toList deck =
    Dict.toList deck


foldr : (Card -> Int -> b -> b) -> b -> DeckSlice -> b
foldr fn result deck =
    Dict.foldr fn result deck


filter : (Card -> Int -> Bool) -> DeckSlice -> DeckSlice
filter fn deck =
    Dict.filter Card.order fn deck


map : (Card -> Int -> b) -> DeckSlice -> List b
map fn deck =
    let
        append card count result =
            (fn card count) :: result
    in
        foldr append [] deck


insert : Card -> Int -> DeckSlice -> DeckSlice
insert card count deck =
    Dict.insert Card.order card count deck


update : Card -> (Maybe Int -> Maybe Int) -> DeckSlice -> DeckSlice
update card fn deck =
    Dict.update Card.order card fn deck


count : Card -> DeckSlice -> Int
count card deck =
    Dict.get Card.order card deck
        |> Maybe.withDefault 0


sum : DeckSlice -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck


toHtml : (Card -> Int -> Html msg) -> DeckSlice -> Html msg
toHtml renderChild deck =
    case isEmpty deck of
        True ->
            Html.Helpers.nothing

        False ->
            div [ class "list-item-grid" ] (map renderChild deck)
