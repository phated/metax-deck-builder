module Data.Card exposing (Card, CardRarity(..), CardType(..), BattleType(..), fromCardType, decoder)

import Regex exposing (regex, contains, find, Regex, Match)
import Json.Decode exposing (int, string, nullable, map, field, map4, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom)

type CardRarity
    = Common
    | Uncommon
    | Rare
    | XRare
    | URare
    | Promo
    | Starter

type BattleType
    = Strength Int
    | Intelligence Int
    | Special Int
    | Multi Int

type CardType
    = Character
    | Event
    | Battle BattleType

type alias Card =
    { id : String
    , title : String
    , card_type : Maybe CardType
    , trait : String
    , mp : Int
    , effect : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    , rarity : Maybe CardRarity
    }


type alias BattleCard =
    { card_type : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int }


decoder : Decoder Card
decoder =
    decode Card
        |> required "id" string
        |> required "title" string
        |> custom (map toCardType battleCard)
        |> required "trait" string
        |> required "mp" int
        |> required "effect" string
        |> required "strength" (nullable int)
        |> required "intelligence" (nullable int)
        |> required "special" (nullable int)
        |> required "image_url" string
        |> custom (field "id" (map toRarity string))


-- Utilities for improved decoding
battleCard : Decoder BattleCard
battleCard =
    map4 BattleCard
        (field "card_type" string)
        (field "strength" (nullable int))
        (field "intelligence" (nullable int))
        (field "special" (nullable int))

fromCardType : Maybe CardType -> String
fromCardType cardType =
    case cardType of
        Just Character ->
            "Character"
        Just Event ->
            "Event"
        Just (Battle _) ->
            "Battle"
        Nothing ->
            ""


toCardType : BattleCard -> Maybe CardType
toCardType {card_type, strength, intelligence, special} =
    case card_type of
        "Character" ->
            Just Character
        "Event" ->
            Just Event
        "Battle" ->
            case (strength, intelligence, special) of
                (Just strRank, Nothing, Nothing) ->
                    Just (Battle (Strength strRank))
                (Nothing, Just intRank, Nothing) ->
                    Just (Battle (Intelligence intRank))
                (Nothing, Nothing, Just spRank) ->
                    Just (Battle (Special spRank))
                (Just multiRank, Just _, Nothing) ->
                    -- TODO: there should probably be some more validation here
                    Just (Battle (Multi multiRank))
                (Just multiRank, Nothing, Just _) ->
                    -- TODO: there should probably be some more validation here
                    Just (Battle (Multi multiRank))
                (Nothing, Just multiRank, Just _) ->
                    -- TODO: there should probably be some more validation here
                    Just (Battle (Multi multiRank))
                (Just multiRank, Just _, Just _) ->
                    -- TODO: there should probably be some more validation here
                    Just (Battle (Multi multiRank))
                (Nothing, Nothing, Nothing) ->
                    Nothing
        _ ->
            Nothing


-- TODO: How can this be more type safe?
matchToRarity : Match -> Maybe CardRarity
matchToRarity match =
    case match.submatches of
        [Just "C"] ->
            Just Common
        [Just "U"] ->
            Just Uncommon
        [Just "R"] ->
            Just Rare
        [Just "XR"] ->
            Just XRare
        [Just "UR"] ->
            Just URare
        [Just "P"] ->
            Just Promo
        [Just "S"] ->
            Just Starter
        [] ->
            Nothing
        _ ->
            Nothing


toRarity : String -> Maybe CardRarity
toRarity id =
    let
        match = List.head (find (Regex.AtMost 1) (regex "^(C|U(?!R)|R|XR|UR|P|S)") id)
    in
        case match of
            Just (m) ->
                matchToRarity m
            Nothing ->
                Nothing
