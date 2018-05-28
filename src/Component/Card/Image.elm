module Component.Card.Image exposing (Image, decoder)

{-| Component.Card.Image


# Types

@docs Image


# Build


# Encoders/Decoders


# Views

-}

import Json.Decode exposing (string, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


{-| -}
type alias Image =
    { original : String
    , large : String
    , medium : String
    , small : String
    , thumbnail : String
    }


decoder : Decoder Image
decoder =
    decode Image
        |> required "original" string
        |> required "large" string
        |> required "medium" string
        |> required "small" string
        |> required "thumbnail" string
