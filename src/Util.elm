module Util exposing (onNavigate)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode exposing (succeed)


onNavigate : a -> Attribute a
onNavigate msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (succeed msg)
