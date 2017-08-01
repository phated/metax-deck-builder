module Data.CardList exposing (CardList, decoder)

import Json.Decode exposing (Decoder, list)
import Data.Card as Card exposing (Card)


type alias CardList =
    List Card


decoder : Decoder CardList
decoder =
    list Card.decoder
