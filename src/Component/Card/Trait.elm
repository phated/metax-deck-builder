module Component.Card.Trait
    exposing
        ( Trait
        , fromString
        , decoder
        , toHtml
        )

{-| Component.Card.Trait represents the trait of a card.


# Types

@docs Trait


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (Decoder, field, string, null, oneOf, andThen, succeed)


{-| The trait of a card or None if not a Character.
-}
type Trait
    = Trait String
    | None


{-| Create a Trait from a String.
-}
fromString : String -> Trait
fromString value =
    Trait value


{-| Decode a trait sub-object into a Trait or None if null.
-}
decoder : Decoder Trait
decoder =
    oneOf
        [ field "name" string |> andThen (fromString >> succeed)
        , null None
        ]


{-| Render a Trait as an Html view. Creates a stub div (for CSS Grid rendering) if None.
-}
toHtml : Trait -> Html msg
toHtml trait =
    let
        children =
            case trait of
                Trait value ->
                    [ text value ]

                None ->
                    []
    in
        div [ class "card-trait" ] children
