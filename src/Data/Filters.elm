module Data.Filters exposing (Filters, applyFilters, empty, fromList, toString, add, remove, member)

import GraphQl as Gql exposing (Value, Query)
import Data.Filter as Filter exposing (Filter(FilterRarity, FilterSet))


type alias Filters =
    { rarity : List Filter
    , set : List Filter
    }


empty : Filters
empty =
    { rarity = [], set = [] }


fromList : List Filter -> Filters
fromList filters =
    List.foldr add empty filters


add : Filter -> Filters -> Filters
add filter filters =
    case filter of
        FilterRarity _ ->
            { filters | rarity = filter :: filters.rarity }

        FilterSet _ ->
            { filters | set = filter :: filters.set }


remove : Filter -> Filters -> Filters
remove filter filters =
    case filter of
        FilterRarity _ ->
            { filters | rarity = List.filter ((/=) filter) filters.rarity }

        FilterSet _ ->
            { filters | set = List.filter ((/=) filter) filters.set }


member : Filter -> Filters -> Bool
member filter filters =
    case filter of
        FilterRarity _ ->
            List.member filter filters.rarity

        FilterSet _ ->
            List.member filter filters.set


toString : Filters -> String
toString filters =
    let
        rarityQuery =
            if List.length filters.rarity == 0 then
                ""
            else
                (++) "rarity:" (String.join "," <| List.map Filter.toString filters.rarity)

        setQuery =
            if List.length filters.set == 0 then
                ""
            else
                (++) "set:" (String.join "," <| List.map Filter.toString filters.set)
    in
        -- TODO: this adds the space before if no rarities selected
        rarityQuery ++ " " ++ setQuery



-- TODO: Need to add a toUrlSearch function
-- toUrlSearch :


applyFilters : Filters -> Value Query -> Value Query
applyFilters filters query =
    List.foldr (\filter q -> q |> Filter.toQuery filter) query (List.concat [ filters.rarity, filters.set ])
