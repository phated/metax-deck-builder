module Data.CardRarity exposing (CardRarity(..), decoder, fromString, toString, toInt)

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


fromString : String -> Maybe CardRarity
fromString str =
    case str of
        "C" ->
            Just Common

        "U" ->
            Just Uncommon

        "R" ->
            Just Rare

        "XR" ->
            Just XRare

        "UR" ->
            Just URare

        "P" ->
            Just Promo

        "S" ->
            Just Starter

        _ ->
            Nothing


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
stringToCardRarity str =
    case fromString str of
        Just rarity ->
            succeed rarity

        Nothing ->
            fail "Invalid card rarity."
