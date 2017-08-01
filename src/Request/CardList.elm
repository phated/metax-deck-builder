module Request.CardList exposing (load)

import Http
import Data.CardList as CardList exposing (CardList)


load : Http.Request CardList
load =
    Http.get "/data/metax.normalized.json" CardList.decoder
