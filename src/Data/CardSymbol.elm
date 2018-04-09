module Data.CardSymbol
    exposing
        ( CardSymbol(..)
        , decoder
        , fromString
        , toHtml
        )

import Html exposing (Html, img, text)
import Html.Attributes exposing (src, class)
import Json.Decode exposing (Decoder, string, andThen, succeed, fail)
import Util exposing (decoderFromMaybe)


type CardSymbol
    = Play
    | Push
    | Constant
    | Attack
    | Defend
    | None


decoder : Decoder CardSymbol
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card symbol.")


fromString : String -> Maybe CardSymbol
fromString value =
    case value of
        "CONSTANT" ->
            Just Constant

        "PLAY" ->
            Just Play

        "PUSH" ->
            Just Push

        "ATTACK" ->
            Just Attack

        "DEFEND" ->
            Just Defend

        "NONE" ->
            Just None

        _ ->
            Nothing


toHtml : CardSymbol -> Html msg
toHtml symbol =
    case symbol of
        Play ->
            img [ class "effect-symbol", src "/icons/play.png" ] []

        Push ->
            img [ class "effect-symbol", src "/icons/push.png" ] []

        Constant ->
            img [ class "effect-symbol upscale", src "/icons/constant.png" ] []

        Attack ->
            img [ class "effect-symbol", src "/icons/attack.png" ] []

        Defend ->
            img [ class "effect-symbol", src "/icons/defend.png" ] []

        None ->
            text ""
