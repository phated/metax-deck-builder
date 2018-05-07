module Component.Deck.Battles
    exposing
        ( Battles
        , foldr
        , filter
        , insert
        , update
        , count
        , sum
        , toList
        )

import Avl.Dict as Dict
import Component.Card as Card exposing (Card)
import Component.Card.Stats exposing (Stats(Single, Multi))
import Component.Card.StatType exposing (StatType(Strength, Intelligence, Special))
import Component.Deck.Battles.Strength as StrengthBattles exposing (StrengthBattles)
import Component.Deck.Battles.Intelligence as IntelligenceBattles exposing (IntelligenceBattles)
import Component.Deck.Battles.Special as SpecialBattles exposing (SpecialBattles)
import Component.Deck.Battles.Multi as MultiBattles exposing (MultiBattles)


type alias Battles a =
    { a
        | strength : StrengthBattles
        , intelligence : IntelligenceBattles
        , special : SpecialBattles
        , multi : MultiBattles
    }


toList : Battles a -> List ( Card, Int )
toList deck =
    List.concat
        [ StrengthBattles.toList deck
        , IntelligenceBattles.toList deck
        , SpecialBattles.toList deck
        , MultiBattles.toList deck
        ]


foldr : (Card -> Int -> b -> b) -> b -> Battles a -> b
foldr fn result deck =
    let
        subfold rank dict result =
            Dict.foldr fn result dict
    in
        StrengthBattles.foldr subfold result deck
            |> flip (IntelligenceBattles.foldr subfold) deck
            |> flip (SpecialBattles.foldr subfold) deck
            |> flip (MultiBattles.foldr subfold) deck


filter : (Card -> Int -> Bool) -> Battles a -> Battles a
filter fn deck =
    deck
        |> StrengthBattles.filter fn
        |> IntelligenceBattles.filter fn
        |> SpecialBattles.filter fn
        |> MultiBattles.filter fn


insert : Card -> Int -> Battles a -> Battles a
insert card count deck =
    case card.stats of
        Single { statType, rank } ->
            case statType of
                Strength ->
                    StrengthBattles.insert rank ( card, count ) deck

                Intelligence ->
                    IntelligenceBattles.insert rank ( card, count ) deck

                Special ->
                    SpecialBattles.insert rank ( card, count ) deck

        Multi { rank } ->
            MultiBattles.insert rank ( card, count ) deck

        _ ->
            deck


update : Card -> (Maybe Int -> Maybe Int) -> Battles a -> Battles a
update card fn deck =
    case card.stats of
        Single { statType, rank } ->
            case statType of
                Strength ->
                    StrengthBattles.update rank ( card, fn ) deck

                Intelligence ->
                    IntelligenceBattles.update rank ( card, fn ) deck

                Special ->
                    SpecialBattles.update rank ( card, fn ) deck

        Multi { rank } ->
            MultiBattles.update rank ( card, fn ) deck

        _ ->
            deck


count : Card -> Battles a -> Int
count card deck =
    case card.stats of
        Single { statType, rank } ->
            case statType of
                Strength ->
                    StrengthBattles.count rank card deck

                Intelligence ->
                    IntelligenceBattles.count rank card deck

                Special ->
                    SpecialBattles.count rank card deck

        Multi { rank } ->
            MultiBattles.count rank card deck

        _ ->
            0


sum : Battles a -> Int
sum deck =
    (StrengthBattles.sum deck) + (IntelligenceBattles.sum deck) + (SpecialBattles.sum deck) + (MultiBattles.sum deck)
