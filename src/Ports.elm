-- Straight from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Ports.elm


port module Ports exposing (onSessionLoaded, loadSession, storeSession, exportSession, ogImage)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port loadSession : String -> Cmd msg


port onSessionLoaded : (Value -> msg) -> Sub msg


port exportSession : Maybe String -> Cmd msg


port ogImage : String -> Cmd msg
