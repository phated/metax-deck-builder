module Data.CardStatList
    exposing
        ( CardStatList
        , decoder
        )

import Json.Decode exposing (Decoder, list)
import Component.Card.Stat as CardStat exposing (Stat)


type alias CardStatList =
    List Stat


decoder : Decoder CardStatList
decoder =
    list CardStat.decoder
