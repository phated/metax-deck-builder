module Data.Card exposing (Card, decoder)

import Json.Decode exposing (int, string, nullable, field, maybe, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom, optional, optionalAt)
import Data.CardSet as CardSet exposing (CardSet)
import Data.CardType as CardType exposing (CardType)
import Data.CardEffect as CardEffect exposing (CardEffect)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardStatList as CardStatList exposing (CardStatList)
import Data.CardPreview as CardPreview exposing (CardPreview)


type alias Card =
    { uid : String
    , set : CardSet
    , number : Int
    , rarity : CardRarity
    , title : String
    , subtitle : Maybe String
    , card_type : CardType
    , trait : String
    , mp : Int
    , effect : CardEffect
    , stats : CardStatList
    , image_url : String
    , preview : Maybe CardPreview
    }


decoder : Decoder Card
decoder =
    decode Card
        |> required "uid" string
        |> custom (field "set" CardSet.decoder)
        |> required "number" int
        |> required "rarity" CardRarity.decoder
        |> required "title" string
        |> optional "subtitle" (maybe string) Nothing
        |> custom (field "type" CardType.decoder)
        |> optionalAt [ "trait", "name" ] string ""
        |> required "mp" int
        |> custom (field "effect" CardEffect.decoder)
        |> custom (field "stats" CardStatList.decoder)
        |> required "imageUrl" string
        |> optional "preview" (maybe CardPreview.decoder) Nothing
