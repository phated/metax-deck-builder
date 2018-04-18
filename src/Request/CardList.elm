module Request.CardList exposing (load, toNamedQuery, allCards)

import Component.CardList as CardList exposing (CardList)
import GraphQl as Gql exposing (Operation, Query, Named, Value)
import Json.Decode exposing (at)


allCards : Value Query
allCards =
    Gql.field "allCards"
        |> Gql.withSelectors
            [ Gql.field "uid"
            , Gql.field "set"
            , Gql.field "number"
            , Gql.field "rarity"
            , Gql.field "mp"
            , Gql.field "type"
            , Gql.field "title"
            , Gql.field "subtitle"
            , Gql.field "trait"
                |> Gql.withSelectors
                    [ Gql.field "name"
                    ]
            , Gql.field "effect"
                |> Gql.withSelectors
                    [ Gql.field "symbol"
                    , Gql.field "text"
                    ]
            , Gql.field "stats"
                |> Gql.withSelectors
                    [ Gql.field "type"
                    , Gql.field "rank"
                    ]
            , Gql.field "imageUrl"
            , Gql.field "preview"
                |> Gql.withSelectors
                    [ Gql.field "previewer"
                    , Gql.field "previewUrl"
                    ]
            ]


toNamedQuery : Value Query -> Operation Query Named
toNamedQuery query =
    Gql.named "AllCards"
        [ query ]


load : Value Query -> Gql.Request Query Named CardList
load query =
    Gql.query
        "https://api.graph.cool/simple/v1/metaxdb"
        (toNamedQuery query)
        (at [ "allCards" ] CardList.decoder)
