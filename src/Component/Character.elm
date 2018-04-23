module Component.Character
    exposing
        ( Character
        , decoder
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (int, string, nullable, field, maybe, at, Decoder)
import Json.Decode.Pipeline exposing (decode, resolve, custom, required, requiredAt, optional, optionalAt)
import Component.Card.UID as CardUID exposing (UID)
import Component.Card.Set as CardSet exposing (Set)
import Component.Card.Effect as CardEffect exposing (Effect)
import Component.Card.Rarity as CardRarity exposing (Rarity)
import Component.Card.Preview as CardPreview exposing (Preview)
import Component.Card.StatList as CardStatList exposing (StatList)
import Component.Card.Stat exposing (Stat(Strength, Intelligence, Special))


type alias Character =
    { uid : UID
    , set : Set
    , number : Int
    , rarity : Rarity
    , title : String
    , subtitle : String
    , trait : String
    , mp : Int
    , effect : Effect
    , stats : StatList
    , image_url : String
    , preview : Maybe Preview
    }


decoder : Decoder Character
decoder =
    decode Character
        |> required "uid" CardUID.decoder
        |> custom (field "set" CardSet.decoder)
        |> required "number" int
        |> required "rarity" CardRarity.decoder
        |> required "title" string
        |> required "subtitle" string
        |> requiredAt [ "trait", "name" ] string
        |> required "mp" int
        |> custom (field "effect" CardEffect.decoder)
        |> custom (field "stats" CardStatList.decoder)
        |> required "imageUrl" string
        |> optional "preview" (maybe CardPreview.decoder) Nothing


toHtml : Character -> Html msg
toHtml { title, subtitle, trait, effect, mp, stats } =
    div [ class "card-details" ]
        [ div [ class "card-text" ]
            [ div [ class "card-title" ] [ text (title ++ " - " ++ subtitle) ]
            , div [ class "card-trait" ] [ text trait ]
            , CardEffect.toHtmlLazy effect
            ]
        , div [ class "card-stats" ] (mpView mp :: CardStatList.toHtml stats)
        ]


mpView : Int -> Html msg
mpView stat =
    let
        prefix =
            if stat >= 0 then
                "+"
            else
                ""
    in
        div [ class "card-stat-mp" ] [ text ("MP" ++ ": " ++ prefix ++ toString stat) ]
