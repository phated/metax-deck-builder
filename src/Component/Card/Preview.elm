module Component.Card.Preview
    exposing
        ( Preview
        , decoder
        , toHtml
        , toHtmlLazy
        )

{-| Component.Card.Preview represents preview metadata for spoiled cards.


# Types

@docs Preview


# Build


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, div, text, a)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (class, href)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
import Html.Helpers


{-| Details about the preview of a card, including the previewer and original URL.
-}
type alias Preview =
    { previewer : String
    , previewUrl : String
    }


{-| Decode a string into a Preview.
-}
decoder : Decoder Preview
decoder =
    decode Preview
        |> required "previewer" string
        |> required "previewUrl" string


{-| Renders the Maybe Preview as an Html view. Produces an empty element if Nothing
-}
toHtml : Maybe Preview -> Html msg
toHtml preview =
    case preview of
        Just preview ->
            div [ class "preview-banner previewed-by" ]
                [ text "Previewed By: "
                , a [ href preview.previewUrl ]
                    [ text preview.previewer ]
                ]

        Nothing ->
            Html.Helpers.nothing


{-| Render the Maybe Preview as a Lazy Html view to avoid re-rendering.
-}
toHtmlLazy : Maybe Preview -> Html msg
toHtmlLazy =
    lazy toHtml
