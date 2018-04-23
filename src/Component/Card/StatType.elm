module Component.Card.StatType
    exposing
        ( StatType(..)
        , fromString
        , decoder
        , toImgUrl
        , collapse
        )

import Json.Decode exposing (Decoder, string, andThen)
import Util exposing (decoderFromMaybe)


{-| The type of a stat on a card. If multi, contains a list of stat types contained by that multi
-}
type StatType
    = Strength
    | Intelligence
    | Special
    | Multi (List StatType)


fromString : String -> Maybe StatType
fromString value =
    case value of
        "Strength" ->
            Just Strength

        "Intelligence" ->
            Just Intelligence

        "Special" ->
            Just Special

        _ ->
            Nothing


decoder : Decoder StatType
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid stat type.")


toImgUrl : StatType -> String
toImgUrl statType =
    case statType of
        Strength ->
            "/icons/strength.png"

        Intelligence ->
            "/icons/intelligence.png"

        Special ->
            "/icons/special.png"

        Multi _ ->
            -- TODO: Vary based on stats
            "/icons/multi.png"


collapse : List StatType -> Maybe StatType
collapse statTypes =
    case statTypes of
        [ s1 ] ->
            Just s1

        [ s1, s2 ] ->
            Just (Multi statTypes)

        [ s1, s2, s3 ] ->
            Just (Multi statTypes)

        _ ->
            Nothing
