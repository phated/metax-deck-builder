module Request.CardList exposing (load)

-- import Http
import Data.CardList as CardList exposing (CardList)

import GraphQl as Gql exposing (Operation, Query, Named)
import Json.Decode exposing (Decoder, field, string, map2, at, list)
import Http exposing (Error)

cardsQuery : Operation Query Named
cardsQuery =
  Gql.named "AllCards" [
    Gql.field "allCards"
      |> Gql.withSelectors [
        Gql.field "uid",
        Gql.field "set",
        Gql.field "number",
        Gql.field "rarity",
        Gql.field "mp",
        Gql.field "type",
        Gql.field "title",
        Gql.field "trait"
          |> Gql.withSelectors [
            Gql.field "name"
          ],
        Gql.field "effect",
        Gql.field "imageUrl",
        Gql.field "preview"
          |> Gql.withSelectors [
              Gql.field "previewer",
              Gql.field "previewUrl"
          ]
      ]
  ]

-- load : Http.Request CardList
-- load =
--     Http.get "/data/metax.normalized.json" CardList.decoder

load = Gql.query
  "https://api.graph.cool/simple/v1/cjerpcdas51ih01414psrg6wa"
  cardsQuery
  (at ["allCards"] CardList.decoder)
