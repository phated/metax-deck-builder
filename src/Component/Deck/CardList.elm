module Component.Deck.CardList
    exposing
        ( CardList
        , empty
        , toList
        , insert
        , sum
        , count
        , foldr
        , foldl
        , increment
        , decrement
        , toHtml
        )

{-| Component.Deck.CardList


# Types

@docs CardList


# Build


# Encoders/Decoders


# Views

-}

import Avl.Dict as Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Helpers
import Component.Card as Card exposing (Card)


{-| -}
type CardList
    = CardList { cards : Dict Card Int, total : Int }


empty : CardList
empty =
    CardList { cards = Dict.empty, total = 0 }


toList : CardList -> List ( Card, Int )
toList (CardList { cards }) =
    Dict.toList cards


insert : Card -> Int -> CardList -> CardList
insert card count (CardList { cards, total }) =
    CardList { cards = Dict.insert Card.order card count cards, total = total + count }


count : Card -> CardList -> Int
count card (CardList { cards }) =
    Dict.get Card.order card cards
        |> Maybe.withDefault 0


sum : CardList -> Int
sum (CardList { total }) =
    total


foldl : (Card -> Int -> a -> a) -> a -> CardList -> a
foldl func acc (CardList { cards }) =
    Dict.foldl func acc cards


foldr : (Card -> Int -> a -> a) -> a -> CardList -> a
foldr func acc (CardList { cards }) =
    Dict.foldr func acc cards


increment : Card -> CardList -> CardList
increment card (CardList { cards, total }) =
    -- TODO: Total is wrong
    CardList { cards = Dict.update Card.order card maybeIncrement cards, total = total }


decrement : Card -> CardList -> CardList
decrement card (CardList { cards, total }) =
    let
        updatedCards =
            Dict.update Card.order card maybeDecrement cards
    in
        -- TODO: Total is wrong
        CardList { cards = Dict.filter Card.order notZero updatedCards, total = total }


toHtml : CardList -> Html msg
toHtml (CardList { cards, total }) =
    if total > 0 then
        div []
            [ div [ class "list-item-header" ] [ text <| "Characters (" ++ (toString total) ++ ")" ]

            -- :: Dict.fold Card.toHtml cards
            ]
    else
        Html.Helpers.nothing



{- Internals -}


notZero : Card -> Int -> Bool
notZero _ count =
    count /= 0


maybeIncrement : Maybe Int -> Maybe Int
maybeIncrement value =
    case value of
        Just value ->
            let
                val =
                    if value < 3 then
                        value + 1
                    else
                        value
            in
                Just val

        Nothing ->
            Just 1


maybeDecrement : Maybe Int -> Maybe Int
maybeDecrement value =
    case value of
        Just value ->
            let
                val =
                    if value > 0 then
                        value - 1
                    else
                        value
            in
                Just val

        Nothing ->
            Just 0
