module Data.CardEffect exposing (CardEffect(..), decoder)

import Regex exposing (regex, find, replace, HowMany(..), Match)
import Json.Decode exposing (field, map, string, Decoder)

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
  field "effect" (map toEffect string)

-- Utils
matchToEffect : Match -> String -> CardEffect
matchToEffect match =
    case match.submatches of
        [Just "PLAY"] ->
            Play
        [Just "PUSH"] ->
            Push
        [Just "CONSTANT"] ->
            Constant
        [Just "ATTACK"] ->
            Attack
        [Just "DEFEND"] ->
            Defend
        [] ->
            Any
        _ ->
            Any

toEffect : String -> CardEffect
toEffect effect =
  let
    symbolRegex = regex "^(PLAY|PUSH|CONSTANT|ATTACK|DEFEND)"
    match = List.head (find (AtMost 1) symbolRegex effect)
    scrubbedEffect =
      replace All symbolRegex (\_ -> "") effect
  in
    case match of
      Just (m) ->
          (matchToEffect m) scrubbedEffect
      Nothing ->
          Any scrubbedEffect
