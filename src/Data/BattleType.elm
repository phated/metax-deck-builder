module Data.BattleType
    exposing
        ( BattleType(..)
        , toInt
        , toBattleType
        )

import Component.Card.Stat as CardStat exposing (Stat)
import Component.Card.StatList exposing (StatList)


-- TODO: Maybe this should be part of CardStatList?


type BattleType
    = Strength Int
    | Intelligence Int
    | Special Int
      -- | StrengthIntelligence Int
      -- | StrengthSpecial Int
      -- | IntelligenceSpecial Int
      -- | StrengthIntelligenceSpecial Int
    | Multi Int


toInt : StatList -> Int
toInt stats =
    case toBattleType stats of
        Just (Strength rank) ->
            0 + rank

        Just (Intelligence rank) ->
            7 + rank

        Just (Special rank) ->
            14 + rank

        Just (Multi rank) ->
            21 + rank

        Nothing ->
            28


battleTypeFoldr : Stat -> Maybe BattleType -> Maybe BattleType
battleTypeFoldr stat battleType =
    case ( battleType, stat ) of
        ( Nothing, CardStat.Strength rank ) ->
            Just (Strength rank)

        ( Nothing, CardStat.Intelligence rank ) ->
            Just (Intelligence rank)

        ( Nothing, CardStat.Special rank ) ->
            Just (Special rank)

        ( Just _, CardStat.Strength rank ) ->
            Just (Multi rank)

        ( Just _, CardStat.Intelligence rank ) ->
            Just (Multi rank)

        ( Just _, CardStat.Special rank ) ->
            Just (Multi rank)


toBattleType : StatList -> Maybe BattleType
toBattleType stats =
    List.foldr battleTypeFoldr Nothing stats
