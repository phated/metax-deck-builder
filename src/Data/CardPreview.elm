module Data.CardPreview
    exposing
        ( CardPreview
        , decoder
        )

import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)


type alias CardPreview =
    { previewer : String
    , previewUrl : String
    }


decoder : Decoder CardPreview
decoder =
    decode CardPreview
        |> required "previewer" string
        |> required "previewUrl" string
