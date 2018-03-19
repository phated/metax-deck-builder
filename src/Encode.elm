module Encode exposing (hash)

import Data.Deck exposing (Deck)
import Data.Card exposing (Card)
import Data.CardRarity exposing (stringToCardRarity, CardRarity(Starter, Common, Uncommon, Rare, XRare, URare, Promo))
import Data.CardSet exposing (stringToCardSet, CardSet(JL, GL, AT))
import Array
import Encoding.Integral exposing (decodeBin, decodeDec, encodeBin)
import Regex exposing (HowMany(All), regex)
import Result
import Dict


-- import Html exposing (Html, text)
-- type alias Card =
--     { qty : Int
--     , cardid : String
--     }


rarityToInt : CardRarity -> Int
rarityToInt rarity =
    case rarity of
        Starter ->
            0

        Common ->
            1

        Uncommon ->
            2

        Rare ->
            3

        Promo ->
            4

        XRare ->
            5

        URare ->
            6


setToInt : CardSet -> Int
setToInt set =
    case set of
        JL ->
            0

        GL ->
            1

        AT ->
            2


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


re =
    regex "^(C|U|R|UR|XR|S|P){1}(\\d*)\\-(JL|GL|AT){1}$"


encodeWithPadding : Int -> Int -> String
encodeWithPadding padLength value =
    String.padLeft padLength '0' <| encodeBin value


encodeQty : Int -> String
encodeQty qty =
    encodeWithPadding 2 (qty - 1)


encodeRarity : CardRarity -> String
encodeRarity rarity =
    let
        val =
            rarityToInt rarity
    in
        encodeWithPadding 3 val


encodeNumber : Int -> String
encodeNumber number =
    encodeWithPadding 8 (number - 1)


encodeSet : CardSet -> String
encodeSet set =
    let
        val =
            setToInt set
    in
        encodeWithPadding 4 val


toBase64 : Maybe Int -> String
toBase64 hashIdx =
    case hashIdx of
        Just idx ->
            let
                asBase64 =
                    Array.get idx sixtyfour
            in
                case asBase64 of
                    Just val ->
                        val

                    Nothing ->
                        ""

        Nothing ->
            ""


encodeCard : ( Maybe Card, Int ) -> String
encodeCard ( maybeCard, qty ) =
    case maybeCard of
        Just card ->
            let
                encodedQty =
                    encodeQty qty

                -- chopped =
                --     Regex.find All re card.uir
                -- onlyOneMatch =
                --     List.head chopped
                rarity =
                    encodeRarity card.rarity

                number =
                    encodeNumber card.number

                set =
                    encodeSet card.set

                -- cardNumber =
                --     case onlyOneMatch of
                --         Just match ->
                --             case match.submatches of
                --                 [ Just rarityMatch, Just numberMatch, Just setMatch ] ->
                --                     [ Maybe.map encodeRarity (stringToCardRarity rarityMatch)
                --                     , encodeNumber numberMatch
                --                     , Maybe.map encodeSet (stringToCardSet setMatch)
                --                     ]
                --                 _ ->
                --                     []
                --         Nothing ->
                --             []
                hash =
                    encodedQty ++ rarity ++ number ++ set

                -- case cardNumber of
                --     [ Just encodedRarity, Just encodedNumber, Just encodedSet ] ->
                --         let
                --             joined =
                --         in
                --             joined
                --     _ ->
                --         ""
                checkbit =
                    (List.length <| String.indexes "1" hash) % 2

                hashWithCheckbit =
                    hash ++ toString checkbit

                sliced =
                    [ String.slice 0 6 hashWithCheckbit
                    , String.slice 6 12 hashWithCheckbit
                    , String.slice 12 18 hashWithCheckbit
                    ]

                decodedSlices =
                    List.map decodeBin sliced

                base64 =
                    String.join "" (List.map toBase64 decodedSlices)

                out =
                    base64
            in
                out

        Nothing ->
            ""


hash : List ( Maybe Card, Int ) -> String
hash deck =
    let
        encodedVersion =
            toBase64 <| Just 0

        encodedCards =
            List.map encodeCard deck

        encodedDeck =
            encodedVersion ++ String.join "" encodedCards

        checksum =
            List.sum (List.map (\v -> Maybe.withDefault 0 (decodeDec v)) (String.split "" encodedDeck))

        base64Checksum =
            toBase64 <| Just (checksum % 64)

        encoded =
            encodedDeck ++ base64Checksum
    in
        -- AiAASAhA
        encoded



-- text <| encoded
