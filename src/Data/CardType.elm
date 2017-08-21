module Data.CardType exposing (BattleType(..), CardType(..), decoder, fromCardType)

import Json.Decode exposing (field, string, nullable, int, map, map4, Decoder)


type BattleType
    = Strength Int
    | Intelligence Int
    | Special Int
    | Multi Int


type CardType
    = Character
    | Event
    | Battle BattleType
    | Unknown


decoder : Decoder CardType
decoder =
    map toCardType battleCard



-- Utils


fromCardType : CardType -> String
fromCardType cardType =
    case cardType of
        Character ->
            "Character"

        Event ->
            "Event"

        Battle _ ->
            "Battle"

        Unknown ->
            ""



-- Just a container to make toCardType easier


type alias BattleCard =
    { card_type : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    }


battleCard : Decoder BattleCard
battleCard =
    map4 BattleCard
        (field "card_type" string)
        (field "strength" (nullable int))
        (field "intelligence" (nullable int))
        (field "special" (nullable int))


toCardType : BattleCard -> CardType
toCardType { card_type, strength, intelligence, special } =
    case card_type of
        "Character" ->
            Character

        "Event" ->
            Event

        "Battle" ->
            toBattleType strength intelligence special

        _ ->
            Unknown



-- TODO: maybe this should be its own type so I can use Unkonwn


toBattleType : Maybe Int -> Maybe Int -> Maybe Int -> CardType
toBattleType strength intelligence special =
    case ( strength, intelligence, special ) of
        ( Just strRank, Nothing, Nothing ) ->
            Battle (Strength strRank)

        ( Nothing, Just intRank, Nothing ) ->
            Battle (Intelligence intRank)

        ( Nothing, Nothing, Just spRank ) ->
            Battle (Special spRank)

        ( Just multiRank, Just _, Nothing ) ->
            -- TODO: there should probably be some more validation here
            Battle (Multi multiRank)

        ( Just multiRank, Nothing, Just _ ) ->
            -- TODO: there should probably be some more validation here
            Battle (Multi multiRank)

        ( Nothing, Just multiRank, Just _ ) ->
            -- TODO: there should probably be some more validation here
            Battle (Multi multiRank)

        ( Just multiRank, Just _, Just _ ) ->
            -- TODO: there should probably be some more validation here
            Battle (Multi multiRank)

        ( Nothing, Nothing, Nothing ) ->
            Unknown
