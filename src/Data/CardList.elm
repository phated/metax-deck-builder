module Data.CardList exposing (CardList, decoder, sort)

import Json.Decode exposing (Decoder, list)
import Compare exposing (concat, by, Comparator)
import Data.Card as Card exposing (Card)
import Data.CardType as CardType exposing (CardType(Battle, Character, Event))
import Data.BattleType as BattleType


type alias CardList =
    List Card


decoder : Decoder CardList
decoder =
    list Card.decoder


sort : CardList -> CardList
sort cards =
    (List.sortWith order cards)



-- Utils


battleTypeOrder : Card -> Int
battleTypeOrder { card_type, stats } =
    case card_type of
        Battle ->
            BattleType.toInt stats

        Character ->
            0

        Event ->
            0


order : Comparator Card
order =
    concat
        [ by (CardType.toInt << .card_type)
        , by battleTypeOrder
        , by .title
        , by (.text << .effect)
        ]
