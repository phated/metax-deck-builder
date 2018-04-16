module Component.IconAttributions
    exposing
        ( IconAttributions
        , defaults
        , toHtml
        , toHtmlLazy
        )

{-| Component.IconAttributions


# Types

@docs IconAttributions


# Build

@docs defaults


# Encoders/Decoders


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, div, ul, text)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (class)
import Component.Attribution as Attribution exposing (Attribution)


{-| A list containing many attributions for icons used in the application.

TODO: Make this an opaque type?

-}
type alias IconAttributions =
    List Attribution


{-| A default list of icons used in the system.
-}
defaults : IconAttributions
defaults =
    [ Attribution "Search Icon" "Icon Depot" "https://thenounproject.com/icon/1165408/"
    , Attribution "Cards Icon" "Daniel Solis" "https://thenounproject.com/icon/219514/"
    , Attribution "Deck Icon" "Michael G Brown" "https://thenounproject.com/icon/1156459/"
    , Attribution "Info Icon" "icongeek" "https://thenounproject.com/icon/808461/"
    ]


{-| Render the IconAttributions as an Html view.
-}
toHtml : IconAttributions -> Html msg
toHtml attributions =
    div [ class "attributions" ]
        [ text "Icons provided by The Noun Project:"
        , ul []
            (List.map Attribution.toHtml attributions)
        ]


{-| Render the IconAttributions as a Lazy Html view to avoid re-rendering if we are using something like defaults.
-}
toHtmlLazy : IconAttributions -> Html msg
toHtmlLazy =
    lazy toHtml
