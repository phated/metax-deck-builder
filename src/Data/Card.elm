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
import Component.Card.UID as CardUID exposing (UID)
import Component.Card.Set as CardSet exposing (Set)
import Component.Card.Type as CardType exposing (Type)
import Component.Card.Effect as CardEffect exposing (Effect)
import Component.Card.Rarity as CardRarity exposing (Rarity)
import Component.Card.Preview as CardPreview exposing (Preview)
import Component.Card.StatList as CardStatList exposing (StatList)


type alias Card =
    { uid : UID
    , set : Set
    , number : Int
    , rarity : Rarity
    , title : String
    , subtitle : Maybe String
    , card_type : Type
    , trait : String
    , mp : Int
    , effect : Effect
    , stats : StatList
    , image_url : String
    , preview : Maybe Preview
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


query : UID -> Value Query
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
