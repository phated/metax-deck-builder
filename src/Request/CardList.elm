module Request.CardList exposing (load)

import Data.CardList as CardList exposing (CardList)
import GraphQl as Gql exposing (Operation, Query, Named)
import Json.Decode exposing (at)


cardsQuery : Operation Query Named
cardsQuery =
    Gql.named "AllCards"
        [ Gql.field "allCards"
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
        ]


load : Gql.Request Query Named CardList
load =
    Gql.query
        "https://api.graph.cool/simple/v1/metaxdb"
        cardsQuery
        (at [ "allCards" ] CardList.decoder)
