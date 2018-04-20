module Component.Card.UID
    exposing
        ( UID
        , fromString
        , eq
        , decoder
        , toString
        , toGql
        )

{-| Component.Card.UID represents the unique ID of a card (e.g. C1-JL).


# Types

@docs UID


# Build

@docs fromString


# Helpers

@docs eq


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toGql

-}

import Json.Decode exposing (Decoder, map, string)


{-| The uid of a card.
-}
type UID
    = UID String


{-| Creates a UID from a String.
-}
fromString : String -> UID
fromString uid =
    UID uid


{-| Check equality between a String and UID. Avoids an extra `fromString` call.
-}
eq : String -> UID -> Bool
eq u1 (UID u2) =
    u1 == u2


{-| Decode a string into a UID.
-}
decoder : Decoder UID
decoder =
    map fromString string


{-| Renders a UID as a String.
-}
toString : UID -> String
toString (UID uid) =
    uid


{-| Renders the UID as a special GraphQL String (wraps it in "").
-}
toGql : UID -> String
toGql (UID uid) =
    "\"" ++ uid ++ "\""
