module Data.Card exposing (Card, decoder)

import Json.Decode exposing (int, string, nullable, field, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom)
import Data.CardType as CardType exposing (CardType)
import Data.CardEffect as CardEffect exposing (CardEffect)
import Data.CardRarity as CardRarity exposing (CardRarity)


type alias Card =
    { id : String
    , set : String
    , title : String
    , card_type : CardType
    , trait : String
    , mp : Int
    , effect : CardEffect
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    , preview_url : String
    , previewer : String
    , rarity : CardRarity
    }


decoder : Decoder Card
decoder =
    decode Card
        |> required "id" string
        |> required "set" string
        |> required "title" string
        |> custom (field "card_type" CardType.decoder)
        |> required "trait" string
        |> required "mp" int
        |> custom CardEffect.decoder
        |> required "strength" (nullable int)
        |> required "intelligence" (nullable int)
        |> required "special" (nullable int)
        |> required "image_url" string
        |> required "preview_url" string
        |> required "previewer" string
        -- TODO: Add a rarity field to the data
        |> custom (field "id" CardRarity.decoder)
