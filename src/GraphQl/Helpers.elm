module GraphQl.Helpers exposing (join)

import GraphQl as Gql exposing (Value)


join : (Value a -> Value a) -> Value a -> Value a
join query1 query2 =
    query1 <| query2
