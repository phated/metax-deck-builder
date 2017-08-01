module Data.Card exposing (Card, decoder)

import Json.Decode exposing (int, string, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Card =
    { id : String
    , title : String
    , card_type : String
    , trait : String
    , mp : Int
    , effect : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    }


decoder : Decoder Card
decoder =
    decode Card
        |> required "id" string
        |> required "title" string
        |> required "card_type" string
        |> required "trait" string
        |> required "mp" int
        |> required "effect" string
        |> required "strength" (nullable int)
        |> required "intelligence" (nullable int)
        |> required "special" (nullable int)
        |> required "image_url" string
