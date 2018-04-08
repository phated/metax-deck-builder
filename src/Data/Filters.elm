module Data.Filters exposing (Filters, Filter(..), applyFilters, empty, fromList, toString, add, remove, member)

import GraphQl as Gql exposing (Value, Query, Argument)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardSet as CardSet exposing (CardSet)
import Data.CardUID as CardUID exposing (CardUID)


type Filter {- Used as the union type over the different filters -}
    = FilterRarity CardRarity
    | FilterSet CardSet
    | FilterUID CardUID


type alias Filters =
    {- Only store the actual type instead of the filter type. Unsure if this is better/worse -}
    { rarity : List CardRarity
    , set : List CardSet
    , uid : List CardUID
    }


empty : Filters
empty =
    Filters [] [] []


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

        FilterUID uid ->
            { filters | uid = uid :: filters.uid }


remove : Filter -> Filters -> Filters
remove filter filters =
    case filter of
        FilterRarity rarity ->
            { filters | rarity = List.filter ((/=) rarity) filters.rarity }

        FilterSet set ->
            { filters | set = List.filter ((/=) set) filters.set }

        FilterUID uid ->
            { filters | uid = List.filter ((/=) uid) filters.uid }


member : Filter -> Filters -> Bool
member filter filters =
    case filter of
        FilterRarity rarity ->
            List.member rarity filters.rarity

        FilterSet set ->
            List.member set filters.set

        FilterUID uid ->
            List.member uid filters.uid


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

        -- TODO: Add UID to the filter string
    in
        -- TODO: this adds the space before if no rarities selected
        rarityQuery ++ " " ++ setQuery



-- TODO: Need to add a toUrlSearch function
-- toUrlSearch :


applyFilters : Filters -> Value Query -> Value Query
applyFilters filters =
    let
        rarityFilters =
            List.map CardRarity.toString filters.rarity

        setFilters =
            List.map CardSet.toString filters.set

        uidFilters =
            List.map CardUID.toGql filters.uid

        activeFilters =
            List.filterMap toGql
                [ ( "rarity_in", rarityFilters )
                , ( "set_in", setFilters )
                , ( "uid_in", uidFilters )
                ]
    in
        -- TODO: handle no filters at all
        Gql.withArgument "filter" <| Gql.queryArgs activeFilters



-- Utils


toGql : ( String, List String ) -> Maybe ( String, Argument Query )
toGql ( key, filters ) =
    if List.length filters == 0 then
        Nothing
    else
        let
            str =
                String.join "," filters
        in
            Just <| ( key, Gql.type_ <| "[" ++ str ++ "]" )
