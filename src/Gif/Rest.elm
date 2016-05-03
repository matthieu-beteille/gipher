module Gif.Rest (..) where

import Gif.Types exposing (..)
import Json.Encode
import Json.Decode as Json

encodeGif : Model -> Json.Encode.Value
encodeGif gif =
  Json.Encode.object
    [ ( "url", Json.Encode.string gif.url )
    , ( "width", Json.Encode.string gif.width )
    , ( "height", Json.Encode.string gif.height )
    , ( "id", Json.Encode.string gif.id )
    ]


decodeGifFromGiphy : Json.Decoder Model
decodeGifFromGiphy =
  Json.object4
    Model
    (Json.at [ "id" ] Json.string)
    (Json.at [ "images", "fixed_height", "url" ] Json.string)
    (Json.at [ "images", "fixed_height", "width" ] Json.string)
    (Json.at [ "images", "fixed_height", "height" ] Json.string)


decodeGifFromFirebase : Json.Decoder Model
decodeGifFromFirebase =
  Json.object4
    Model
    (Json.at [ "id" ] Json.string)
    (Json.at [ "url" ] Json.string)
    (Json.at [ "width" ] Json.string)
    (Json.at [ "height" ] Json.string)
