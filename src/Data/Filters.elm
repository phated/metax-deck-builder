module Data.Filters
    exposing
        ( Filters
        , Filter(..)
        , fromList
        , empty
        , add
        , remove
        , member
        , union
        , toString
        , toQueryString
        , fromQueryString
        , applyFilters
        )

import GraphQl as Gql exposing (Value, Query, Argument)
import Fork.QueryString as QueryString exposing (QueryString)
import Data.CardRarity as CardRarity exposing (CardRarity(..))
import Data.CardSet as CardSet exposing (CardSet(..))
import Data.CardUID as CardUID exposing (CardUID)


type Filter {- Used as the union type over the different filters -}
    = FilterRarity CardRarity
    | FilterSet CardSet
    | FilterUID CardUID


type alias Filters =
    {- Only store the actual type instead of the filter type. Unsure if this is better/worse -}
    {- TODO: Unique lists -}
    { rarity : List CardRarity
    , set : List CardSet
    , uid : List CardUID
    }


empty : Filters
empty =
    Filters [] [] []


default : Filters
default =
    Filters
        [ Common, Uncommon, Rare, XRare, URare ]
        [ AT, GL, JL ]
        []


fromList : List Filter -> Filters
fromList filters =
    List.foldr add empty filters


toList : Filters -> List Filter
toList filters =
    List.concat
        [ List.map FilterRarity filters.rarity
        , List.map FilterSet filters.set
        , List.map FilterUID filters.uid
        ]


add : Filter -> Filters -> Filters
add filter filters =
    case filter of
        FilterRarity rarity ->
            if List.member rarity filters.rarity then
                filters
            else
                { filters | rarity = rarity :: filters.rarity }

        FilterSet set ->
            if List.member set filters.set then
                filters
            else
                { filters | set = set :: filters.set }

        FilterUID uid ->
            if List.member uid filters.uid then
                filters
            else
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


foldr : (Filter -> b -> b) -> b -> Filters -> b
foldr fn result filters =
    List.foldr fn result (toList filters)


union : Filters -> Filters -> Filters
union f1 f2 =
    foldr add f2 f1


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


toQueryString : Filters -> QueryString
toQueryString filters =
    let
        qs =
            QueryString.empty

        rarities =
            List.map CardRarity.toString filters.rarity

        sets =
            List.map CardSet.toString filters.set

        withRarities =
            List.foldr (QueryString.add "rarity") qs rarities

        withSets =
            List.foldr (QueryString.add "set") withRarities sets
    in
        withSets


fromQueryString : QueryString -> Filters
fromQueryString qs =
    let
        rarityStrings =
            qs |> QueryString.all "rarity"

        rarities =
            List.filterMap CardRarity.fromString rarityStrings

        setStrings =
            qs |> QueryString.all "set"

        sets =
            List.filterMap CardSet.fromString setStrings
    in
        if List.isEmpty rarities && List.isEmpty sets then
            default
        else
            Filters rarities sets []


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
            filterString =
                String.join "," filters
        in
            Just <| ( key, Gql.type_ <| "[" ++ filterString ++ "]" )
