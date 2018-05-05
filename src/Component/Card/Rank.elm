module Component.Card.Rank
    exposing
        ( Rank
        , fromInt
        , decoder
        , toInt
        , toString
        , toHtml
        , order
        , ofValue
        , ofValue2
        , ofValue3
        )

{-| Component.Card.Rank represents the rank a stat can be.


# Types

@docs Rank


# Build

@docs fromInt


# Encoders/Decoders

@docs decoder


# Views

@docs toInt, toString, toHtml


# Ordering

@docs order


# Validators

@docs ofValue, ofValue2, ofValue3

-}

import Html exposing (Html, text, span)
import Html.Attributes exposing (class)
import Json.Decode exposing (Decoder, int, andThen, succeed)


{-| The rank of a stat.
-}
type Rank
    = Rank Int


{-| Create a Rank from an Int.
-}
fromInt : Int -> Rank
fromInt value =
    -- TODO: Should there be validation here?
    Rank value


{-| Decode a string into a Rank.
-}
decoder : Decoder Rank
decoder =
    int |> andThen (fromInt >> succeed)


{-| Renders a Rank as an Int. (Really, it just unwraps it.)
-}
toInt : Rank -> Int
toInt (Rank value) =
    value


{-| Renders a Rank as a String.
-}
toString : Rank -> String
toString (Rank value) =
    Basics.toString value


{-| Renders a Rank as an Html view.
-}
toHtml : Rank -> Html msg
toHtml rank =
    span [ class "card-stat-text" ] [ text (toString rank) ]


{-| Convert a Rank into a comparable. This is useful when used with `Compare.by`.
-}
order : Rank -> Int
order (Rank value) =
    -- TODO: Should this be `toInt`?
    value


{-| Validate a Rank and return it if valid.
-}
ofValue : Rank -> Maybe Rank
ofValue rank =
    -- TODO: Should these guard range?
    Just rank


{-| Compare 2 Ranks and return the first if equal.
-}
ofValue2 : Rank -> Rank -> Maybe Rank
ofValue2 rank1 rank2 =
    if eq rank1 rank2 then
        Just rank1
    else
        Nothing


{-| Compare 3 Ranks and return the first if equal.
-}
ofValue3 : Rank -> Rank -> Rank -> Maybe Rank
ofValue3 rank1 rank2 rank3 =
    if eq rank1 rank2 && eq rank2 rank3 && eq rank3 rank1 then
        Just rank1
    else
        Nothing



-- Internals


eq : Rank -> Rank -> Bool
eq (Rank value1) (Rank value2) =
    value1 == value2
