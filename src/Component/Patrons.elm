module Component.Patrons
    exposing
        ( Patrons
        , defaults
        , toHtml
        , toHtmlLazy
        )

{-| Component.Patrons


# Types

@docs Patrons


# Build

@docs defaults


# Encoders/Decoders


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, div, text, img, span, a)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (class, src, href, alt)


{-| A list containing names of our patrons.
-}
type alias Patrons =
    List String


{-| A default list of our patrons. This is not dynamic currently.
-}
defaults : Patrons
defaults =
    [ "Matt Smith" ]


{-| Render the Patrons as an Html view.
-}
toHtml : Patrons -> Html msg
toHtml patrons =
    div [ class "patreon" ]
        [ div [] [ text "Our work is supported by our Patrons on Patreon." ]
        , div []
            [ text "Extra "
            , img [ class "card-stat-icon", src "/icons/special.png" ] []
            , text " thanks to our Rank 5+ Patrons: "
            , span [ class "patreon-rank-5" ] (List.map text patrons)
            ]
        , div [ class "patreon" ] [ text "For development updates and extra features, subscribe to us on Patreon!" ]
        , a [ class "patreon-link", href "https://www.patreon.com/metaxdb" ] [ img [ src "/icons/patron.png", alt "Become a Patron" ] [] ]
        ]


{-| Render the Patrons as a Lazy Html view to avoid re-rendering if we are using something like defaults.
-}
toHtmlLazy : Patrons -> Html msg
toHtmlLazy =
    lazy toHtml
