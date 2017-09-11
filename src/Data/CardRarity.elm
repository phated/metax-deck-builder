module Data.CardRarity exposing (CardRarity(..), decoder, stringToCardRarity, cardRarityToString)

import Regex exposing (find, regex, Match, HowMany(AtMost))
import Json.Decode exposing (string, andThen, Decoder)
import Json.Decode.Extra exposing (fromResult)


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
    string |> andThen (fromString >> fromResult)

fromString : String -> Result String CardRarity
fromString =
    -- TODO: A lot of this can go away when a rarity column is added to the data
    Result.fromMaybe "Invalid card rarity." << toRarity


-- Utils
stringToCardRarity : String -> Maybe CardRarity
stringToCardRarity rarity =
    case rarity of
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


cardRarityToString : CardRarity -> String
cardRarityToString rarity =
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


-- TODO: How can this be more type safe?

matchToRarity : Match -> Maybe CardRarity
matchToRarity match =
    case match.submatches of
        [ Just m ] ->
            stringToCardRarity m

        [] ->
            Nothing

        _ ->
            Nothing


toRarity : String -> Maybe CardRarity
toRarity id =
    let
        match =
            List.head (find (AtMost 1) (regex "^(C|U(?!R)|R|XR|UR|P|S)") id)
    in
        case match of
            Just m ->
                matchToRarity m

            Nothing ->
                Nothing
