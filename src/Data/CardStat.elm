module Data.CardStat exposing (CardStat(..), decoder)

import Json.Decode exposing (map, map2, int, field, string, andThen, Decoder)
import Json.Decode.Extra exposing (fromResult)


type CardStat
    = Strength Int
    | Intelligence Int
    | Special Int


type alias StatRecord =
    { type_ : String
    , rank : Int
    }


decodeRecord : Decoder StatRecord
decodeRecord =
    map2 StatRecord
        (field "type" string)
        (field "rank" int)


decoder : Decoder CardStat
decoder =
    decodeRecord |> andThen (fromRecord >> fromResult)


fromRecord : StatRecord -> Result String CardStat
fromRecord =
    Result.fromMaybe "Invalid card stat." << recordToCardStat



-- Utils


recordToCardStat : StatRecord -> Maybe CardStat
recordToCardStat record =
    case record.type_ of
        "Strength" ->
            Just (Strength record.rank)

        "Intelligence" ->
            Just (Intelligence record.rank)

        "Special" ->
            Just (Special record.rank)

        _ ->
            Nothing
