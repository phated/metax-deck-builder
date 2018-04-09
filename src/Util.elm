module Util exposing (onNavigate, decoderFromMaybe)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode exposing (succeed, fail, Decoder)


onNavigate : a -> Attribute a
onNavigate msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (succeed msg)


decoderFromMaybe : String -> Maybe a -> Decoder a
decoderFromMaybe failMessage maybeValue =
    case maybeValue of
        Just value ->
            succeed value

        Nothing ->
            fail failMessage
