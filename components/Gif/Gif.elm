module Gif where

import Json.Encode
import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)

-- Model

type alias Model =
  { id: String
  , url: String
  , width: String
  , height: String
  , smallWidth: String
  , smallHeight: String }

 -- Encoder / Decoder

encodeGif : Model -> Json.Encode.Value
encodeGif gif =
  Json.Encode.object
    [ ( "url", Json.Encode.string gif.url )
    , ( "width", Json.Encode.string gif.width )
    , ( "height", Json.Encode.string gif.height )
    , ( "smallWidth", Json.Encode.string gif.smallWidth )
    , ( "smallHeight", Json.Encode.string gif.smallHeight )
    , ( "id", Json.Encode.string gif.id ) ]

decodeGifFromGiphy: Json.Decoder Model
decodeGifFromGiphy =
  Json.object6
    Model
    (Json.at ["id"] Json.string)
    (Json.at ["images", "fixed_height", "url"] Json.string)
    (Json.at ["images", "fixed_height", "width"] Json.string)
    (Json.at ["images", "fixed_height", "height"] Json.string)
    (Json.at ["images", "fixed_height_small", "width"] Json.string)
    (Json.at ["images", "fixed_height_small", "height"] Json.string)

decodeGifFromFirebase: Json.Decoder Model
decodeGifFromFirebase =
  Json.object6
    Model
    (Json.at ["id"] Json.string)
    (Json.at ["url"] Json.string)
    (Json.at ["width"] Json.string)
    (Json.at ["height"] Json.string)
    (Json.at ["width"] Json.string)
    (Json.at ["height"] Json.string)

-- View

cardView: Model -> Html
cardView gif =
  div [ getContainerStyle ] [ div [getImgStyle gif] [] ]

-- Style

getContainerStyle: Attribute
getContainerStyle =
  style [ ("border", "1px solid #BBBFBE")
        , ("overflow-x", "hidden")
        , ("padding", "5px")
        , ("background-color", "white")
        , ("cursor", "pointer")
        , ("border-radius", "3px") ]

getImgStyle: Model -> Attribute
getImgStyle model =
  let { width, height, url } = model
  in
    style [ ( "width", "200px" )
          , ( "height", height ++ "px" )
          , ( "backgroundImage", "url(" ++ url ++ ")" )
          , ( "backgroundPosition", "center center" )
          , ( "border-radius", "3px" ) ]
