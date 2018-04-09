module Data.CardSet
    exposing
        ( CardSet(..)
        , decoder
        , fromString
        , toString
        , toInt
        )

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)
import Util exposing (decoderFromMaybe)


type CardSet
    = JL
    | GL
    | AT


decoder : Decoder CardSet
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card set.")


fromString : String -> Maybe CardSet
fromString str =
    case str of
        "JL" ->
            Just JL

        "GL" ->
            Just GL

        "AT" ->
            Just AT

        _ ->
            Nothing


toString : CardSet -> String
toString cardSet =
    case cardSet of
        JL ->
            "JL"

        GL ->
            "GL"

        AT ->
            "AT"


toInt : CardSet -> Int
toInt set =
    case set of
        JL ->
            0

        GL ->
            1

        AT ->
            2
