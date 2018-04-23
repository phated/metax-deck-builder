module Component.Battle
    exposing
        ( Battle
        , decoder
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (int, string, field, maybe, Decoder)
import Json.Decode.Pipeline exposing (decode, custom, required, optional)
import Component.Card.UID as CardUID exposing (UID)
import Component.Card.Set as CardSet exposing (Set)
import Component.Card.Effect as CardEffect exposing (Effect)
import Component.Card.Rarity as CardRarity exposing (Rarity)
import Component.Card.Preview as CardPreview exposing (Preview)


type alias Battle =
    { uid : UID
    , set : Set
    , number : Int
    , rarity : Rarity
    , title : String
    , mp : Int
    , effect : Effect
    , image_url : String
    , preview : Maybe Preview

    {- Non-shared -}
    -- , rank : Int
    -- , stat : BattleType
    }


decoder : Decoder Battle
decoder =
    decode Battle
        |> required "uid" CardUID.decoder
        |> custom (field "set" CardSet.decoder)
        |> required "number" int
        |> required "rarity" CardRarity.decoder
        |> required "title" string
        |> required "mp" int
        |> custom (field "effect" CardEffect.decoder)
        |> required "imageUrl" string
        |> optional "preview" (maybe CardPreview.decoder) Nothing


toHtml : Battle -> Html msg
toHtml { title, effect, mp } =
    div [ class "card-details" ]
        [ div [ class "card-text" ]
            [ div [ class "card-title" ]
                [ text title

                {- , text <| toBattleCardRank card -}
                ]
            , CardEffect.toHtmlLazy effect
            ]
        , div [ class "card-stats" ] [ mpView mp ]
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



{-
   toRank : Stat -> Maybe Int -> Maybe Int
   toRank stat rank =
       case stat of
           Strength rank ->
               Just rank

           Intelligence rank ->
               Just rank

           Special rank ->
               Just rank


   toBattleCardRank : Card -> String
   toBattleCardRank card =
       let
           rank =
               case card of
                   BattleCard card ->
                       List.foldr toRank Nothing []

                   _ ->
                       Nothing
       in
           case rank of
               Just rank ->
                   " - Rank " ++ (toString rank)

               Nothing ->
                   ""
-}
