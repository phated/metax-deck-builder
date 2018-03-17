module Data.CardEffect exposing (CardEffect(..), decoder, effectToString, effectToHtml)

import Html exposing (img, text, Html)
import Html.Attributes exposing (src, class)
import Regex exposing (regex, find, replace, HowMany(..), Match)
import Json.Decode exposing (field, map, string, oneOf, null, Decoder)


type CardEffect
    = Play String
    | Push String
    | Constant String
    | Attack String
    | Defend String
      -- TODO: Any isn't the best name
    | Any String


decoder : Decoder CardEffect
decoder =
    field "effect" (oneOf [
        (map toEffect string),
        null (Any "")
    ])



-- Utils


effectToString : CardEffect -> String
effectToString effect =
    case effect of
        Play content ->
            content

        Push content ->
            content

        Constant content ->
            content

        Attack content ->
            content

        Defend content ->
            content

        Any content ->
            content


effectToImg : CardEffect -> Maybe (Html msg)
effectToImg effect =
    case effect of
        Play content ->
            Just (img [ src "/icons/play.png" ] [])

        Push content ->
            Just (img [ src "/icons/push.png" ] [])

        Constant content ->
            Just (img [ class "upscale", src "/icons/constant.png" ] [])

        Attack content ->
            Just (img [ src "/icons/attack.png" ] [])

        Defend content ->
            Just (img [ src "/icons/defend.png" ] [])

        Any content ->
            Nothing


effectToHtml : CardEffect -> List (Html msg)
effectToHtml effect =
    let
        image =
            effectToImg effect

        content =
            effectToString effect
    in
        case image of
            Just image ->
                [ image
                , text content
                ]

            Nothing ->
                [ text content ]


matchToEffect : Match -> String -> CardEffect
matchToEffect match =
    case match.submatches of
        [ Just "PLAY" ] ->
            Play

        [ Just "PUSH" ] ->
            Push

        [ Just "CONSTANT" ] ->
            Constant

        [ Just "ATTACK" ] ->
            Attack

        [ Just "DEFEND" ] ->
            Defend

        [] ->
            Any

        _ ->
            Any


toEffect : String -> CardEffect
toEffect effect =
    let
        symbolRegex =
            regex "^(PLAY|PUSH|CONSTANT|ATTACK|DEFEND)"

        match =
            List.head (find (AtMost 1) symbolRegex effect)

        scrubbedEffect =
            replace All symbolRegex (\_ -> "") effect
    in
        case match of
            Just m ->
                (matchToEffect m) scrubbedEffect

            Nothing ->
                Any scrubbedEffect
