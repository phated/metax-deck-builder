module Data.Card
    exposing
        ( Card
        , decoder
        , query
        , load
        )

import Json.Decode exposing (int, string, nullable, field, maybe, at, Decoder)
import Json.Decode.Pipeline exposing (decode, required, custom, optional, optionalAt)
import GraphQl as Gql exposing (Value, Query, Anonymous, object)
import Data.CardSet as CardSet exposing (CardSet)
import Data.CardType as CardType exposing (CardType)
import Data.CardRarity as CardRarity exposing (CardRarity)
import Data.CardStatList as CardStatList exposing (CardStatList)
import Data.CardUID as CardUID exposing (CardUID)
import Component.CardEffect as CardEffect exposing (CardEffect)
import Component.CardPreview as CardPreview exposing (CardPreview)


type alias Card =
    { uid : CardUID
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
        |> required "uid" CardUID.decoder
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


query : CardUID -> Value Query
query uid =
    Gql.field "Card"
        |> Gql.withArgument "uid" (CardUID.toString uid |> Gql.string)
        |> Gql.withSelectors
            [ Gql.field "uid"
            , Gql.field "set"
            , Gql.field "number"
            , Gql.field "rarity"
            , Gql.field "mp"
            , Gql.field "type"
            , Gql.field "title"
            , Gql.field "subtitle"
            , Gql.field "trait"
                |> Gql.withSelectors
                    [ Gql.field "name"
                    ]
            , Gql.field "effect"
                |> Gql.withSelectors
                    [ Gql.field "symbol"
                    , Gql.field "text"
                    ]
            , Gql.field "stats"
                |> Gql.withSelectors
                    [ Gql.field "type"
                    , Gql.field "rank"
                    ]
            , Gql.field "imageUrl"
            , Gql.field "preview"
                |> Gql.withSelectors
                    [ Gql.field "previewer"
                    , Gql.field "previewUrl"
                    ]
            ]


load : Value Query -> Gql.Request Query Anonymous Card
load query =
    Gql.query
        "https://api.graph.cool/simple/v1/metaxdb"
        (object [ query ])
        (at [ "Card" ] decoder)
