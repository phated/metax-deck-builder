module Request.Deck exposing (save)

import Json.Encode exposing (encode, string, int)
import Json.Helpers exposing (encodeMap)

import Data.Deck exposing (Deck)

import Ports

save : Deck -> Cmd msg
save deck =
  encodeMap string int deck
    |> encode 0
    |> Just
    |> Ports.storeSession
