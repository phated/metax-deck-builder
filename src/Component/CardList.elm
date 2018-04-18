module Component.CardList
    exposing
        ( CardList
        , empty
        , fromList
        , map
        , decoder
        , toList
        )

{-| Component.CardList represents a unique list of cards.


# Types

@docs CardList


# Build

@docs fromList


# Helpers (TODO: Better name?)

@docs getByUID


# Encoders/Decoders

@docs decoder


# Views

-}

import Avl.Set as Set exposing (Set)
import Json.Decode exposing (Decoder, list, andThen, succeed)
import Compare exposing (concat, by, Comparator)
import Data.BattleType as BattleType
import Component.Card as Card exposing (Card)
import Component.Card.Type as CardType exposing (Type(Battle, Character, Event))


{-| A unique list of cards.
-}
type alias CardList =
    Set Card


empty : CardList
empty =
    Set.empty


{-| Creates a CardList from a List of Cards
-}
fromList : List Card -> CardList
fromList cards =
    Set.fromList order cards


map : (Card -> a) -> CardList -> List a
map mapper cards =
    Set.foldr (mapper >> (::)) [] cards


{-| Decode a string into a CardList
-}
decoder : Decoder CardList
decoder =
    list Card.decoder |> andThen (fromList >> succeed)


toList : CardList -> List Card
toList cards =
    Set.toList cards



-- Utils
-- TODO: Dedupe sorting


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
