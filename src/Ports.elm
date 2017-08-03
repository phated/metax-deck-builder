-- Straight from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Ports.elm


port module Ports exposing (onSessionChange, storeSession, exportSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


port exportSession : Maybe String -> Cmd msg
