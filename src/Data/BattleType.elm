module Data.BattleType
    exposing
        ( BattleType(..)
        , toInt
        , toBattleType
        )

import Component.Card.Stat as CardStat exposing (Stat)
import Component.Card.Stats as Stats exposing (Stats)
import Component.Card.StatType as StatType exposing (StatType(..))


-- TODO: Maybe this should be part of CardStats?


type BattleType
    = Strength Int
    | Intelligence Int
    | Special Int
      -- | StrengthIntelligence Int
      -- | StrengthSpecial Int
      -- | IntelligenceSpecial Int
      -- | StrengthIntelligenceSpecial Int
    | Multi Int


toInt : Stats -> Int
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
    case ( battleType, stat.stat_type ) of
        ( Nothing, StatType.Strength ) ->
            Just (Strength stat.rank)

        ( Nothing, StatType.Intelligence ) ->
            Just (Intelligence stat.rank)

        ( Nothing, StatType.Special ) ->
            Just (Special stat.rank)

        ( Nothing, StatType.Multi _ ) ->
            Just (Multi stat.rank)

        ( Just _, StatType.Strength ) ->
            Just (Multi stat.rank)

        ( Just _, StatType.Intelligence ) ->
            Just (Multi stat.rank)

        ( Just _, StatType.Special ) ->
            Just (Multi stat.rank)

        ( Just _, StatType.Multi _ ) ->
            Just (Multi stat.rank)


toBattleType : Stats -> Maybe BattleType
toBattleType stats =
    Stats.foldr battleTypeFoldr Nothing stats
