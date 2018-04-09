module Data.CardSet exposing (CardSet(..), decoder, fromString, toString, toInt)

import Json.Decode exposing (string, andThen, succeed, fail, Decoder)


type CardSet
    = JL
    | GL
    | AT


decoder : Decoder CardSet
decoder =
    string |> andThen stringToCardSet


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



-- Utils


stringToCardSet : String -> Decoder CardSet
stringToCardSet str =
    case fromString str of
        Just set ->
            succeed set

        Nothing ->
            fail "Invalid card set."
