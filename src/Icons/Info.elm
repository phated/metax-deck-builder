module Icons.Info exposing (view)

import Html exposing (Html)
import Svg exposing (Attribute, svg, path, circle)
import Svg.Attributes exposing (class, d, x, y, viewBox, stroke, fill, cx, cy, r)


-- <svg
--   xmlns="http://www.w3.org/2000/svg"
--   xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" x="0px" y="0px" viewBox="0 0 96 120" style="enable-background:new 0 0 96 96;" xml:space="preserve">
--   <g>
--     <g>
--       <path d="M48,92C23.7,92,4,72.3,4,48S23.7,4,48,4s44,19.7,44,44S72.3,92,48,92z M48,12c-19.9,0-36,16.1-36,36    s16.1,36,36,36s36-16.1,36-36S67.9,12,48,12z"/>
--     </g>
--     <g>
--       <g>
--         <path d="M48,71c-2.2,0-4-1.8-4-4V44c0-2.2,1.8-4,4-4s4,1.8,4,4v23C52,69.2,50.2,71,48,71z"/>
--       </g>
--       <g>
--         <circle cx="48" cy="31" r="4"/>
--         <path d="M48,36c-2.8,0-5-2.2-5-5s2.2-5,5-5s5,2.2,5,5S50.8,36,48,36z M48,28c-1.7,0-3,1.3-3,3s1.3,3,3,3     s3-1.3,3-3S49.7,28,48,28z"/>
--       </g>
--     </g>
--   </g>
-- </svg>


pathOne : Attribute msg
pathOne =
    d "M48,92C23.7,92,4,72.3,4,48S23.7,4,48,4s44,19.7,44,44S72.3,92,48,92z M48,12c-19.9,0-36,16.1-36,36    s16.1,36,36,36s36-16.1,36-36S67.9,12,48,12z"


pathTwo : Attribute msg
pathTwo =
    d "M48,71c-2.2,0-4-1.8-4-4V44c0-2.2,1.8-4,4-4s4,1.8,4,4v23C52,69.2,50.2,71,48,71z"


pathThree : Attribute msg
pathThree =
    d "M48,36c-2.8,0-5-2.2-5-5s2.2-5,5-5s5,2.2,5,5S50.8,36,48,36z M48,28c-1.7,0-3,1.3-3,3s1.3,3,3,3     s3-1.3,3-3S49.7,28,48,28z"


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
        [ path [ pathOne ] []
        , path [ pathTwo ] []
        , circle [ cx "48", cy "31", r "4" ] []
        , path [ pathThree ] []
        ]
