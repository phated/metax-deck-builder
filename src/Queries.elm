module Queries exposing (getCards, Msg)

import GraphQl as GQL exposing (Operation, Query, Named)
import Json.Decode exposing (Decoder, field, string, map2, at, list)
import Http exposing (Error)

cardsQuery : Operation Query Named
cardsQuery =
  GQL.named "AllCards" [
    GQL.field "allCards"
      |> GQL.withSelectors [
        GQL.field "uid",
        GQL.field "title"
      ]
  ]

type alias Card = {
  id: String,
  title: String
}

decoder : Decoder (List Card)
decoder = list (map2 Card (field "uid" string) (field "title" string))

type Msg = GraphQlMsg (Result Error (List Card))

getCards : Cmd Msg
getCards = GQL.query
  "https://api.graph.cool/simple/v1/cjerpcdas51ih01414psrg6wa"
  cardsQuery
  (at ["allCards"] decoder)
  |> GQL.send GraphQlMsg
