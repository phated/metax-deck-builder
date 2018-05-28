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
        , query
        )

import GraphQl as Gql exposing (Value, Query, Argument)
import Fork.QueryString as QueryString exposing (QueryString)
import Component.Card.UID as CardUID exposing (UID)
import Component.Card.Set as CardSet exposing (Set(JL, GL, AT, BM))
import Component.Card.Rarity as CardRarity exposing (Rarity(Common, Uncommon, Rare, XRare, URare, Promo, Starter))


type Filter {- Used as the union type over the different filters -}
    = FilterRarity Rarity
    | FilterSet Set
    | FilterUID UID


type alias Filters =
    {- Only store the actual type instead of the filter type. Unsure if this is better/worse -}
    {- TODO: Unique lists -}
    { rarity : List Rarity
    , set : List Set
    , uid : List UID
    }


empty : Filters
empty =
    Filters [] [] []


default : Filters
default =
    Filters
        [ Common, Uncommon, Rare, XRare, URare ]
        [ AT, GL, JL, BM ]
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
                mapRarityStrings filters.rarity
                    |> String.join ","
                    |> (++) "r:"

        setQuery =
            if List.length filters.set == 0 then
                ""
            else
                mapSetStrings filters.set
                    |> String.join ","
                    |> (++) "s:"

        -- TODO: Add UID to the filter string
    in
        -- TODO: this adds the space before if no rarities selected
        rarityQuery ++ " " ++ setQuery


toQueryString : Filters -> QueryString
toQueryString filters =
    QueryString.empty
        |> QueryString.addBy (mapRarityStrings >> joiner) "r" filters.rarity
        |> QueryString.addBy (mapSetStrings >> joiner) "s" filters.set


fromQueryString : QueryString -> Filters
fromQueryString qs =
    let
        rarities =
            qs |> QueryString.allBy (splitter >> List.filterMap CardRarity.fromString) "r"

        sets =
            qs |> QueryString.allBy (splitter >> List.filterMap CardSet.fromString) "s"
    in
        if List.isEmpty rarities && List.isEmpty sets then
            default
        else
            Filters rarities sets []


query : Filters -> (Value Query -> Value Query)
query filters =
    let
        rarityFilters =
            mapRarityStrings filters.rarity

        setFilters =
            mapSetStrings filters.set

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


joiner : List String -> Maybe String
joiner values =
    if List.length values == 0 then
        Nothing
    else
        Just (String.join "," values)


splitter : String -> List String
splitter =
    String.split ","


mapRarityStrings : List Rarity -> List String
mapRarityStrings =
    List.map CardRarity.toString


mapSetStrings : List Set -> List String
mapSetStrings =
    List.map CardSet.toString
