module Gif where

import Json.Decode as Json exposing ((:=))
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Debug
import Signal
import Mouse

type alias Gif = { url: String
                  , width: String
                  , height: String }

type alias AnimationModel = { x: Int }

type alias Model = { animationState: AnimationModel
                    , gif: Gif }

type Action = Test
 | MousePos (Int, Int)

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
update action model =
  case action of
    Test -> { model | animationState = (Debug.log "model" (AnimationModel (model.animationState.x + 10))) }
    MousePos (a,b) -> { model | animationState = (AnimationModel (a)) }

view: Signal.Address Action -> Model -> Html
view address model =
  let state = Debug.log "lol" model.animationState
      -- test = Signal.map (send address) Mouse.position
  in
    img (getGifAttributes model address) []

send: Signal.Address Action -> (Int, Int) -> Task x ()
send address (a, b) =
  Signal.send address Test

getGifAttributes: Model -> Signal.Address Action -> List (Attribute)
getGifAttributes model address =
  [ src model.gif.url
  , onClick address Test
  -- , onMouseDown Debug.log "loool"
  , style [ ("width", model.gif.width)
          , ("height", model.gif.height)
          , ("transform", "translateX(" ++ (toString model.animationState.x) ++ "px)")
          ]]
