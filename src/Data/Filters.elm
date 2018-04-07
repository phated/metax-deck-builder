module Data.Filters exposing (Filters, Filter(..), applyFilters, empty, fromList, toString, add, remove, member)

import GraphQl as Gql exposing (Value, Query)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardSet as CardSet exposing (CardSet)


type Filter {- Used as the union type over the different filters -}
    = FilterRarity CardRarity
    | FilterSet CardSet


type alias Filters =
    {- Only store the actual type instead of the filter type. Unsure if this is better/worse -}
    { rarity : List CardRarity
    , set : List CardSet
    }


empty : Filters
empty =
    Filters [] []


fromList : List Filter -> Filters
fromList filters =
    List.foldr add empty filters


add : Filter -> Filters -> Filters
add filter filters =
    case filter of
        FilterRarity rarity ->
            { filters | rarity = rarity :: filters.rarity }

        FilterSet set ->
            { filters | set = set :: filters.set }


remove : Filter -> Filters -> Filters
remove filter filters =
    case filter of
        FilterRarity rarity ->
            { filters | rarity = List.filter ((/=) rarity) filters.rarity }

        FilterSet set ->
            { filters | set = List.filter ((/=) set) filters.set }


member : Filter -> Filters -> Bool
member filter filters =
    case filter of
        FilterRarity rarity ->
            List.member rarity filters.rarity

        FilterSet set ->
            List.member set filters.set


toString : Filters -> String
toString filters =
    let
        rarityQuery =
            if List.length filters.rarity == 0 then
                ""
            else
                (++) "rarity:" (String.join "," <| List.map CardRarity.toString filters.rarity)

        setQuery =
            if List.length filters.set == 0 then
                ""
            else
                (++) "set:" (String.join "," <| List.map CardSet.toString filters.set)
    in
        -- TODO: this adds the space before if no rarities selected
        rarityQuery ++ " " ++ setQuery



-- TODO: Need to add a toUrlSearch function
-- toUrlSearch :


applyFilters : Filters -> Value Query -> Value Query
applyFilters filters =
    let
        rarityFilters =
            String.join "," <| List.map CardRarity.toString filters.rarity

        setFilters =
            String.join "," <| List.map CardSet.toString filters.set
    in
        Gql.withArgument "filter" <|
            Gql.queryArgs
                [ ( "rarity_in", Gql.type_ <| "[" ++ rarityFilters ++ "]" )
                , ( "set_in", Gql.type_ <| "[" ++ setFilters ++ "]" )
                ]
