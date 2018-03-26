module Icons.OneCard exposing (view)

import Html exposing (Html)
import Svg exposing (Attribute, svg, path)
import Svg.Attributes exposing (class, d, x, y, viewBox, stroke, fill)


-- <svg
--   xmlns="http://www.w3.org/2000/svg"
--   xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" x="0px" y="0px" viewBox="0 0 100 125" enable-background="new 0 0 100 100" xml:space="preserve">
--   <path d="M75.713,5H23.354c-4.481,0-8.114,3.697-8.114,8.184v73.699c0,4.487,3.633,8.118,8.12,8.118h52.359  c4.48,0,8.114-3.631,8.114-8.118V13.118C83.833,8.631,80.199,5,75.713,5z M75.719,87.782H23.36c-0.503,0-0.905-0.409-0.905-0.9  V13.184c0-0.491,0.404-0.966,0.9-0.966h52.359c0.503,0,0.905,0.409,0.905,0.9v73.699C76.619,87.307,76.214,87.782,75.719,87.782z"/>
-- </svg>


pathD : Attribute msg
pathD =
    d "M75.713,5H23.354c-4.481,0-8.114,3.697-8.114,8.184v73.699c0,4.487,3.633,8.118,8.12,8.118h52.359  c4.48,0,8.114-3.631,8.114-8.118V13.118C83.833,8.631,80.199,5,75.713,5z M75.719,87.782H23.36c-0.503,0-0.905-0.409-0.905-0.9  V13.184c0-0.491,0.404-0.966,0.9-0.966h52.359c0.503,0,0.905,0.409,0.905,0.9v73.699C76.619,87.307,76.214,87.782,75.719,87.782z"


view : String -> Html msg
view color =
    svg
        [ x "0px"
        , y "0px"
        , viewBox "0 0 100 100"
        , stroke color
        , fill color
        , class "navbar-icon"
        ]
        [ path [ pathD ] [] ]
