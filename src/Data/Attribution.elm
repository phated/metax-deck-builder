module Data.Attribution exposing (Attribution, toHtml)

import Html exposing (Html, li, a, text)
import Html.Attributes exposing (href)


type alias Attribution =
    { name : String
    , author : String
    , link : String
    }


toHtml : Attribution -> Html msg
toHtml attribution =
    li []
        [ a [ href attribution.link ]
            [ text attribution.name
            , text " by "
            , text attribution.author
            ]
        ]
