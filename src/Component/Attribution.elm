module Component.Attribution
    exposing
        ( Attribution
        , toHtml
        )

{-| Component.Attribution represenets an attribution for something we've used in the system
and includes the name of the thing we're using, the author and a link to it.


# Types

@docs Attribution


# Encoders/Decoders


# Views

@docs toHtml

-}

import Html exposing (Html, li, a, text)
import Html.Attributes exposing (href, rel)


{-| A record containing the name, author and link to something we are using that needs to be attributed.

    Attribution "Some Icon" "John Doe" "http://example.com"

-}
type alias Attribution =
    { name : String
    , author : String
    , link : String
    }


{-| Render the Attribution as an Html view.

    Attribution.toHtml (Attribution "Some Icon" "John Doe" "http://example.com") == li [] [ ... ]

-}
toHtml : Attribution -> Html msg
toHtml attribution =
    li []
        [ a [ rel "nofollow", href attribution.link ]
            [ text attribution.name
            , text " by "
            , text attribution.author
            ]
        ]
