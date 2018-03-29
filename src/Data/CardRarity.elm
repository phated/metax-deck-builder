module Data.CardRarity exposing (CardRarity(..), decoder, toString, toInt)

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)


type CardRarity
    = Common
    | Uncommon
    | Rare
    | XRare
    | URare
    | Promo
    | Starter


decoder : Decoder CardRarity
decoder =
    string |> andThen stringToCardRarity


toString : CardRarity -> String
toString rarity =
    case rarity of
        Common ->
            "C"

        Uncommon ->
            "U"

        Rare ->
            "R"

        XRare ->
            "XR"

        URare ->
            "UR"

        Promo ->
            "P"

        Starter ->
            "S"


toInt : CardRarity -> Int
toInt rarity =
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



-- Utils


stringToCardRarity : String -> Decoder CardRarity
stringToCardRarity rarity =
    case rarity of
        "C" ->
            succeed Common

        "U" ->
            succeed Uncommon

        "R" ->
            succeed Rare

        "XR" ->
            succeed XRare

        "UR" ->
            succeed URare

        "P" ->
            succeed Promo

        "S" ->
            succeed Starter

        _ ->
            fail "Invalid card rarity."
