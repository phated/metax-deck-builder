module Data.CardUID
    exposing
        ( CardUID
        , toString
        , fromString
        , decoder
        , toGql
        )

import Json.Decode exposing (Decoder, map, string)


type CardUID
    = CardUID String


decoder : Decoder CardUID
decoder =
    map fromString string


fromString : String -> CardUID
fromString uid =
    CardUID uid


toString : CardUID -> String
toString uid =
    case uid of
        CardUID uid ->
            uid


toGql : CardUID -> String
toGql uid =
    "\"" ++ toString uid ++ "\""
