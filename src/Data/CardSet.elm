module Data.CardSet exposing (CardSet(..), decoder, stringToCardSet, cardSetToString)

import Json.Decode exposing (string, andThen, Decoder)
import Json.Decode.Extra exposing (fromResult)


type CardSet
    = JL
    | GL
    | AT


decoder : Decoder CardSet
decoder =
    string |> andThen (fromString >> fromResult)


fromString : String -> Result String CardSet
fromString =
    Result.fromMaybe "Invalid card set." << stringToCardSet



-- Utils


stringToCardSet : String -> Maybe CardSet
stringToCardSet cardSet =
    case cardSet of
        "JL" ->
            Just JL

        "GL" ->
            Just GL

        "AT" ->
            Just AT

        _ ->
            Nothing


cardSetToString : CardSet -> String
cardSetToString cardSet =
    case cardSet of
        JL ->
            "JL"

        GL ->
            "GL"

        AT ->
            "AT"
