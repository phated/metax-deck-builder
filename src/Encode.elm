module Encode exposing (encodeCard, toBase64, encodeChecksum)

import Array exposing (Array)
import Component.Card exposing (Card)
import Component.Card.Set as CardSet exposing (Set)
import Component.Card.Rarity as CardRarity exposing (Rarity)


type alias EncodeResult =
    { cardHashes : List String
    , checksum : Int
    }


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


encodeRarity : Rarity -> Int
encodeRarity rarity =
    -- TODO: Should this be a View in Rarity Component?
    let
        val =
            CardRarity.toInt rarity
    in
        val * 8192


encodeNumber : Int -> Int
encodeNumber number =
    (number - 1) * 32


encodeSet : Set -> Int
encodeSet set =
    -- TODO: Should this be a View in Set Component?
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


encodeChecksum : String -> Int
encodeChecksum val =
    case val of
        "0" ->
            0

        "1" ->
            1

        "2" ->
            2

        "3" ->
            3

        "4" ->
            4

        "5" ->
            5

        "6" ->
            6

        "7" ->
            7

        "8" ->
            8

        "9" ->
            9

        _ ->
            0


encodeCard : Card -> Int -> EncodeResult -> EncodeResult
encodeCard card qty result =
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

        {- Checkbit is no longer checked -}
        checkbit =
            0

        hashWithCheckbit =
            hash + checkbit

        sextet3 =
            hashWithCheckbit % 64

        sextet2 =
            (floor ((toFloat <| hashWithCheckbit - sextet3) / 64)) % 64

        sextet1 =
            (floor ((toFloat <| hashWithCheckbit - sextet2 - sextet3) / 4096)) % 64

        encodedSextet1 =
            toBase64 sextet1

        encodedSextet2 =
            toBase64 sextet2

        encodedSextet3 =
            toBase64 sextet3

        checksum =
            (encodeChecksum encodedSextet1) + (encodeChecksum encodedSextet2) + (encodeChecksum encodedSextet3)

        cardHash =
            encodedSextet1 ++ encodedSextet2 ++ encodedSextet3
    in
        { cardHashes = cardHash :: result.cardHashes, checksum = (result.checksum + checksum) }
