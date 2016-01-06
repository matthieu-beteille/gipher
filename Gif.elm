module Gif where

import Json.Decode as Json exposing ((:=))
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)

type alias Gif = { url: String
                  , width: String
                  , height: String }

type alias AnimationModel = { x: Int }

type alias Model = { animationState: AnimationModel
                    , gif: Gif }

type Action = Test

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2 Model decodeAnimationModel decodeGif

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object1 AnimationModel (Json.succeed 0)

decodeGif: Json.Decoder Gif
decodeGif =
  Json.object3 Gif (Json.at ["images", "fixed_width", "url"] Json.string)
                  (Json.at ["images", "fixed_width", "width"] Json.string)
                  (Json.at ["images", "fixed_width", "height"] Json.string)

update: Action -> Model -> Model


view: Signal.Address Action -> Model -> Html
view address model =
  img (getGifAttributes model.gif) []

getGifAttributes: Gif -> List (Attribute)
getGifAttributes gif =
  [ src gif.url
  -- , onMouseDown Debug.log "loool"
  , style [ ("width", gif.width)
          , ("height", gif.height)
          ]]
