module Data.CardSymbol exposing (CardSymbol(..), decoder, toHtml)

import Html exposing (Html, img, text)
import Html.Attributes exposing (src, class)
import Json.Decode exposing (Decoder, string, andThen, succeed, fail)


type CardSymbol
    = Play
    | Push
    | Constant
    | Attack
    | Defend
    | None


decoder : Decoder CardSymbol
decoder =
    string |> andThen stringToCardSymbol


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



-- Utils


stringToCardSymbol : String -> Decoder CardSymbol
stringToCardSymbol val =
    case val of
        "CONSTANT" ->
            succeed Constant

        "PLAY" ->
            succeed Play

        "PUSH" ->
            succeed Push

        "ATTACK" ->
            succeed Attack

        "DEFEND" ->
            succeed Defend

        "NONE" ->
            succeed None

        _ ->
            fail "Invalid card symbol."
