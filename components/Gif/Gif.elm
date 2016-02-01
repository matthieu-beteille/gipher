module Gif where

import Json.Encode
import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Regex

-- Model

type alias Model =
  { id: String
  , url: String
  , width: String
  , height: String }

 -- Encoder / Decoder

encodeGif : Model -> Json.Encode.Value
encodeGif gif =
  Json.Encode.object
    [ ( "url", Json.Encode.string gif.url )
    , ( "width", Json.Encode.string gif.width )
    , ( "height", Json.Encode.string gif.height )
    , ( "id", Json.Encode.string gif.id ) ]

decodeGifFromGiphy: Json.Decoder Model
decodeGifFromGiphy =
  Json.object4
    Model
    (Json.at ["id"] Json.string)
    (Json.at ["images", "fixed_height", "url"] Json.string)
    (Json.at ["images", "fixed_height", "width"] Json.string)
    (Json.at ["images", "fixed_height", "height"] Json.string)

decodeGifFromFirebase: Json.Decoder Model
decodeGifFromFirebase =
  Json.object4
    Model
    (Json.at ["id"] Json.string)
    (Json.at ["url"] Json.string)
    (Json.at ["width"] Json.string)
    (Json.at ["height"] Json.string)

-- View

cardView: Model -> Html
cardView gif =
  div [ getContainerStyle ] [ div  [ getImgStyle gif ] [] ]

stackView: Int -> Model -> Html
stackView index model =
  div [ stackCardStyle model index ] [ div [ getContainerStyle ] [ div [ getImgStyle model ] [] ] ]

smallView: Model -> Html
smallView gif =
  img  [ src (getFixedWidthUrl gif.url)
       , style [ ( "margin", "0.5px" )
               , ( "border-radius", "2px" ) ] ] []

-- modalView: Model -> Html
-- cardView gif =
--   div [ getModalStyle ] [ div [ getImgStyle gif ] [] ]
-- Style

getFixedWidthUrl =
  Regex.replace Regex.All (Regex.regex "[.]gif") (\_ -> "w.gif")

getContainerStyle: Attribute
getContainerStyle =
  style [ ("box-shadow", "0px 0px 4px 0px #0076E5" )
        , ("overflow-x", "hidden")
        , ("padding", "5px")
        , ("background-color", "white")
        , ("cursor", "pointer")
        , ("border-radius", "3px") ]

getModalStyle: Attribute
getModalStyle =
  style [ ("position", "fixed")
        , ("padding", "10px")
        , ("background-color", "white")
        , ("border-radius", "3px") ]

stackCardStyle: Model -> Int -> Attribute
stackCardStyle gif index =
  let height = gif.height
            |> String.toInt
            |> Result.toMaybe
            |> Maybe.withDefault 200
      scaleRatio = 1 - (((toFloat index) + 1) * 0.01)
      offsetX = toString 40
      offsetY = toString (13 + height - (3 * (1 + index)) + 40)
  in
    style [ ( "position", "absolute" )
          , ( "transform", "translate3d(" ++ offsetX ++ "px, -" ++ offsetY ++ "px, 0px) scale(" ++ (toString scaleRatio) ++ ")") ]

getImgStyle: Model -> Attribute
getImgStyle model =
  let { width, height, url } = model
  in
    style [ ( "width", "200px" )
          , ( "height", height ++ "px" )
          , ( "backgroundImage", "url(" ++ url ++ ")" )
          , ( "backgroundPosition", "center center" )
          , ( "border-radius", "3px" ) ]
