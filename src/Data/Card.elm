module Data.Card exposing (Card, decoder)

import Json.Decode exposing (int, string, nullable, map, field, map4, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom)

import Data.CardType as CardType exposing (CardType)
import Data.CardRarity as CardRarity exposing (CardRarity)

type alias Card =
    { id : String
    , title : String
    , card_type : CardType
    , trait : String
    , mp : Int
    , effect : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    , rarity : CardRarity
    }


decoder : Decoder Card
decoder =
    decode Card
        |> required "id" string
        |> required "title" string
        |> custom CardType.decoder
        |> required "trait" string
        |> required "mp" int
        |> required "effect" string
        |> required "strength" (nullable int)
        |> required "intelligence" (nullable int)
        |> required "special" (nullable int)
        |> required "image_url" string
        |> custom CardRarity.decoder
