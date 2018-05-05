module Component.Card.MP
    exposing
        ( MP
        , fromInt
        , decoder
        , toString
        , toHtml
        )

{-| Component.Card.MP represents the MetaPoints of a Card.


# Types

@docs MP


# Build

@docs fromInt


# Encoders/Decoders

@docs decoder


# Views

@docs toString, toHtml

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (Decoder, int, andThen, succeed)


{-| The MetaPoints of a card.
-}
type MP
    = MP Int


{-| Create MP from an Int.
-}
fromInt : Int -> MP
fromInt value =
    MP value


{-| Decode a string into MP.
-}
decoder : Decoder MP
decoder =
    int |> andThen (fromInt >> succeed)


{-| Render MP as a String.
-}
toString : MP -> String
toString (MP value) =
    let
        str =
            Basics.toString value
    in
        case (value >= 0) of
            True ->
                "+" ++ str

            False ->
                str


{-| Render MP as an Html view.
-}
toHtml : MP -> Html msg
toHtml mp =
    div [ class "card-stat-mp" ]
        [ text <| "MP: " ++ (toString mp) ]
