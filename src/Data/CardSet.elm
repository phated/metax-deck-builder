module Data.CardSet exposing (CardSet(..), decoder, toString, toInt)

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)


type CardSet
    = JL
    | GL
    | AT


decoder : Decoder CardSet
decoder =
    string |> andThen stringToCardSet


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



-- Utils


stringToCardSet : String -> Decoder CardSet
stringToCardSet cardSet =
    case cardSet of
        "JL" ->
            succeed JL

        "GL" ->
            succeed GL

        "AT" ->
            succeed AT

        _ ->
            fail "Invalid card set."
