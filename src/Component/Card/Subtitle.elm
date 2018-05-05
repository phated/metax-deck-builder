module Component.Card.Subtitle
    exposing
        ( Subtitle
        , fromString
        , decoder
        , toHtml
        )

{-| Component.Card.Subtitle represents the subtitle of a card.


# Types

@docs Subtitle


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (Html, text)
import Json.Decode exposing (Decoder, string, null, oneOf, andThen, succeed)
import Html.Helpers


{-| The subtitle of a card or None if it is not a Character.
-}
type Subtitle
    = Subtitle String
    | None


{-| Create a Subtitle from a String.
-}
fromString : String -> Subtitle
fromString value =
    Subtitle value


{-| Decode a string into a Subtitle or None if null.
-}
decoder : Decoder Subtitle
decoder =
    oneOf
        [ string |> andThen (fromString >> succeed)
        , null None
        ]


{-| Render a Subtitle as an Html view.
-}
toHtml : Subtitle -> Html msg
toHtml subtitle =
    case subtitle of
        Subtitle value ->
            text (" - " ++ value)

        None ->
            Html.Helpers.nothing
