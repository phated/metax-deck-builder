module Component.CardSymbol
    exposing
        ( CardSymbol(..)
        , fromString
        , decoder
        , toHtml
        , toHtmlLazy
        )

{-| Component.CardSymbol represents a symbol on a card.


# Types

@docs CardSymbol


# Build

@docs fromString


# Encoders/Decoders

@docs decoder


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, img, text)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (src, class)
import Json.Decode exposing (Decoder, string, andThen, succeed, fail)
import Util exposing (decoderFromMaybe)


{-| The symbol on a card.
-}
type CardSymbol
    = Play
    | Push
    | Constant
    | Attack
    | Defend
    | None


{-| Create a CardSymbol from a String. Will be Nothing if the string is an invalid value.
-}
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


{-| Decode a string into a CardSymbol.
-}
decoder : Decoder CardSymbol
decoder =
    string |> andThen (fromString >> decoderFromMaybe "Invalid card symbol.")


{-| Renders the CardSymbol as an Html view.
-}
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


{-| Render the CardSymbol as a Lazy Html view to avoid re-rendering.

TODO: Does this work for each value in our union type?

-}
toHtmlLazy : CardSymbol -> Html msg
toHtmlLazy =
    lazy toHtml
