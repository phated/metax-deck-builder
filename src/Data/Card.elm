module Data.Card exposing (Card, decoder)

import Json.Decode exposing (int, string, nullable, field, maybe, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom, optional, optionalAt)
import Data.CardType as CardType exposing (CardType)
import Data.CardEffect as CardEffect exposing (CardEffect)
import Data.CardRarity as CardRarity exposing (CardRarity)

type alias CardPreview =
    { previewer : String
    , previewUrl : String
    }

previewDecoder : Decoder CardPreview
previewDecoder =
    decode CardPreview
        |> required "previewer" string
        |> required "previewUrl" string

type alias Card =
    { uid : String
    , set : String
    , number : Int
    , rarity : CardRarity
    , title : String
    , card_type : CardType
    , trait : String
    , mp : Int
    , effect : CardEffect
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    , preview : Maybe CardPreview
    }


decoder : Decoder Card
decoder =
    decode Card
        |> required "uid" string
        |> required "set" string
        |> required "number" int
        |> required "rarity" CardRarity.decoder
        |> required "title" string
        |> custom (field "type" CardType.decoder)
        |> optionalAt ["trait", "name"] string ""
        |> required "mp" int
        |> custom CardEffect.decoder
        |> optional "strength" (nullable int) Nothing
        |> optional "intelligence" (nullable int) Nothing
        |> optional "special" (nullable int) Nothing
        |> required "imageUrl" string
        |> optional "preview" (maybe previewDecoder) Nothing
