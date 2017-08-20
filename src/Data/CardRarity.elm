module Data.CardRarity exposing (CardRarity(..), decoder)

import Regex exposing (find, regex, Match, HowMany(AtMost))
import Json.Decode exposing (string, map, field, Decoder)

type CardRarity
    = Common
    | Uncommon
    | Rare
    | XRare
    | URare
    | Promo
    | Starter
    | Unknown

decoder : Decoder CardRarity
decoder =
  field "id" (map toRarity string)


-- Utils
-- TODO: How can this be more type safe?
matchToRarity : Match -> CardRarity
matchToRarity match =
    case match.submatches of
        [Just "C"] ->
            Common
        [Just "U"] ->
            Uncommon
        [Just "R"] ->
            Rare
        [Just "XR"] ->
            XRare
        [Just "UR"] ->
            URare
        [Just "P"] ->
            Promo
        [Just "S"] ->
            Starter
        [] ->
            Unknown
        _ ->
            Unknown


toRarity : String -> CardRarity
toRarity id =
    let
        match = List.head (find (AtMost 1) (regex "^(C|U(?!R)|R|XR|UR|P|S)") id)
    in
        case match of
            Just (m) ->
                matchToRarity m
            Nothing ->
                Unknown
