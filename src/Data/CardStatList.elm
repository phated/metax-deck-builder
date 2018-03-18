module Data.CardStatList exposing (CardStatList, decoder)

import Json.Decode exposing (Decoder, list)
import Data.CardStat as CardStat exposing (CardStat)


type alias CardStatList =
    List CardStat


decoder : Decoder CardStatList
decoder =
    list CardStat.decoder
