module Util exposing (onNavigate, decoderFromMaybe, finder)

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


finder : (a -> Bool) -> a -> Maybe a -> Maybe a
finder predicate val result =
    case result of
        Just _ ->
            result

        Nothing ->
            if predicate val then
                Just val
            else
                Nothing
