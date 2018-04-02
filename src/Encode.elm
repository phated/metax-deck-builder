module Encode exposing (hash)

import Data.Card exposing (Card)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardSet as CardSet exposing (CardSet)
import Array exposing (Array)
import Encoding.Integral exposing (decodeBin, decodeDec, encodeBin)


sixtyfour : Array String
sixtyfour =
    Array.fromList
        [ "A"
        , "B"
        , "C"
        , "D"
        , "E"
        , "F"
        , "G"
        , "H"
        , "I"
        , "J"
        , "K"
        , "L"
        , "M"
        , "N"
        , "O"
        , "P"
        , "Q"
        , "R"
        , "S"
        , "T"
        , "U"
        , "V"
        , "W"
        , "X"
        , "Y"
        , "Z"
        , "a"
        , "b"
        , "c"
        , "d"
        , "e"
        , "f"
        , "g"
        , "h"
        , "i"
        , "j"
        , "k"
        , "l"
        , "m"
        , "n"
        , "o"
        , "p"
        , "q"
        , "r"
        , "s"
        , "t"
        , "u"
        , "v"
        , "w"
        , "x"
        , "y"
        , "z"
        , "0"
        , "1"
        , "2"
        , "3"
        , "4"
        , "5"
        , "6"
        , "7"
        , "8"
        , "9"
        , "-"
        , "_"
        ]


encodeQty : Int -> Int
encodeQty qty =
    (qty - 1) * 65536


encodeRarity : CardRarity -> Int
encodeRarity rarity =
    let
        val =
            CardRarity.toInt rarity
    in
        val * 8192


encodeNumber : Int -> Int
encodeNumber number =
    (number - 1) * 32


encodeSet : CardSet -> Int
encodeSet set =
    let
        val =
            CardSet.toInt set
    in
        val * 2


toBase64 : Int -> String
toBase64 hashIdx =
    let
        asBase64 =
            Array.get hashIdx sixtyfour
    in
        case asBase64 of
            Just val ->
                val

            Nothing ->
                ""


encodeCard : ( Card, Int ) -> String
encodeCard ( card, qty ) =
    let
        encodedQty =
            encodeQty qty

        rarity =
            encodeRarity card.rarity

        number =
            encodeNumber card.number

        set =
            encodeSet card.set

        hash =
            encodedQty + rarity + number + set

        checkbit =
            (List.length <| String.indexes "1" <| encodeBin hash) % 2

        hashWithCheckbit =
            hash + checkbit

        sextet3 =
            hashWithCheckbit % 64

        sextet2 =
            (floor ((toFloat <| hashWithCheckbit - sextet3) / 64)) % 64

        sextet1 =
            (floor ((toFloat <| hashWithCheckbit - sextet2 - sextet3) / 4096)) % 64

        base64 =
            (toBase64 sextet1) ++ (toBase64 sextet2) ++ (toBase64 sextet3)

        out =
            base64
    in
        out


hash : List ( Card, Int ) -> Maybe String
hash deck =
    let
        encodedVersion =
            toBase64 0

        encodedCards =
            List.map encodeCard deck

        encodedDeck =
            encodedVersion ++ String.join "" encodedCards

        checksum =
            List.sum (List.map (\v -> Maybe.withDefault 0 (decodeDec v)) (String.split "" encodedDeck))

        base64Checksum =
            toBase64 <| checksum % 64

        encoded =
            encodedDeck ++ base64Checksum
    in
        -- AiAASAhA
        if List.length encodedCards > 0 then
            Just encoded
        else
            Nothing
