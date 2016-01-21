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
import String
import Result
import Maybe
import Easing exposing (ease, easeOutBounce, float)

type alias Gif = { url: String
                  , width: String
                  , height: String }

type alias AnimationModel = { isClicked: Bool
                            , x: Int
                            , y: Int
                            , dx: Int
                            , dy: Int}

type alias Model = { animationState: AnimationModel
                    , gif: Gif }

type Action = MousePos (Int, Int)
  | DragStart (Int, Int)
  | DragEnd

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2 Model decodeAnimationModel decodeGif

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object5 AnimationModel (Json.succeed False)
                              (Json.succeed 0)
                              (Json.succeed 0)
                              (Json.succeed 0)
                              (Json.succeed 0)

decodeGif: Json.Decoder Gif
decodeGif =
  Json.object3 Gif (Json.at ["images", "fixed_width", "url"] Json.string)
                  (Json.at ["images", "fixed_width", "width"] Json.string)
                  (Json.at ["images", "fixed_width", "height"] Json.string)

update: Action -> Model -> Model
update action model =
  let { x, y, dx, dy, isClicked } = model.animationState
  in
    case action of
      MousePos (a, b) ->
        if isClicked then
          let dx = Debug.log "dx" (a - x)
              dy = Debug.log "dy" (b - y)
          in
            { model | animationState = (AnimationModel isClicked x y dx dy) }
        else
          model

      DragStart (a, b) ->
        let test = Debug.log "mousepositionx" x
            test2 = Debug.log "pagex" a
            test3 = Debug.log "mousepositiony" y
            ewq = Debug.log "pagey" b
        in
          { model | animationState = (AnimationModel True a b dx dy) }

      DragEnd ->
        { model | animationState = (AnimationModel False x y dx dy) }


-- toOffset : AnimationState -> Float
-- toOffset animationState =
--   ease easeOutBounce float 0 200 second elapsedTime


view: Signal.Address Action -> Model -> Html
view address model =
  div (getGifAttributes model address) []

decoder =
  Json.object2 (,)
    ("pageX" := Json.int)
    ("pageY" := Json.int)

translate3d x y =
  ("transform", "translate3d(" ++ x ++ "px, " ++ y ++ "px, 0)")

getGifAttributes: Model -> Signal.Address Action -> List (Attribute)
getGifAttributes model address =
  [ Html.Events.on "mousedown" decoder (\val -> Signal.message address (DragStart val))
  , onMouseUp address DragEnd
  , style (getStyle model) ]

getStyle: Model -> List ((String, String))
getStyle model =
  let { width, height } = model.gif
      { dx, dy, isClicked } = model.animationState
      transform = if isClicked
      then translate3d (toString dx) (toString dy)
      else translate3d "0" "0"
  in
    transform :: [ ("width", model.gif.width ++ "px")
    , ("height", model.gif.height ++ "px")
    , ("backgroundImage", "url(" ++ model.gif.url ++ ")")
    , ("cursor", "pointer")
    ]
