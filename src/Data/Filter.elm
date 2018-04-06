module Data.Filter exposing (Filter(..), toString, toQuery)

import GraphQl as Gql exposing (Value, Query)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardSet as CardSet exposing (CardSet)


type Filter
    = FilterRarity CardRarity
    | FilterSet CardSet


toString : Filter -> String
toString filter =
    case filter of
        FilterRarity rarity ->
            CardRarity.toString rarity

        FilterSet set ->
            CardSet.toString set


toQuery : Filter -> Value Query -> Value Query
toQuery filter =
    case filter of
        -- TODO: these are wrong
        FilterRarity rarity ->
            Gql.withArgument "rarity_in" <| Gql.type_ (CardRarity.toString rarity)

        FilterSet set ->
            Gql.withArgument "set" <| Gql.type_ (CardSet.toString set)
