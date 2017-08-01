module Data.Deck exposing (Deck, decoder)

import Dict exposing (Dict)
import Json.Decode exposing (int, string, decodeValue, decodeString, dict, Decoder, Value)

type alias Deck =
    Dict String Int

decoder : Value -> Deck
decoder session =
    session
        |> decodeValue string
        |> Result.andThen (decodeString (dict int))
        |> Result.withDefault Dict.empty
