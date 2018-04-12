module Data.Attribution
    exposing
        ( Attribution
        , toHtml
        )

import Html exposing (Html, li, a, text)
import Html.Attributes exposing (href, rel)


type alias Attribution =
    { name : String
    , author : String
    , link : String
    }


toHtml : Attribution -> Html msg
toHtml attribution =
    li []
        [ a [ rel "nofollow", href attribution.link ]
            [ text attribution.name
            , text " by "
            , text attribution.author
            ]
        ]
