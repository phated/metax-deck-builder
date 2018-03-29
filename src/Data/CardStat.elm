module Data.CardStat exposing (CardStat(..), decoder)

import Json.Decode exposing (int, string, succeed, fail, Decoder)
import Json.Decode.Pipeline exposing (decode, required, resolve)


type CardStat
    = Strength Int
    | Intelligence Int
    | Special Int


decoder : Decoder CardStat
decoder =
    decode toCardStat
        |> required "type" string
        |> required "rank" int
        |> resolve



-- Utils


toCardStat : String -> Int -> Decoder CardStat
toCardStat type_ rank =
    case type_ of
        "Strength" ->
            succeed (Strength rank)

        "Intelligence" ->
            succeed (Intelligence rank)

        "Special" ->
            succeed (Special rank)

        _ ->
            fail "Invalid card stat."
