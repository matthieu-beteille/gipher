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
import Basics
import Easing exposing (..)
import Time exposing (Time, millisecond)

type alias Gif = { url: String
                  , width: String
                  , height: String }

type alias AnimationModel = { isClicked: Bool
                            , elapsedTime: Time
                            , prevClockTime: Time
                            , x: Int
                            , y: Int
                            , dx: Int
                            , dy: Int }


type alias Model = { animationState: AnimationModel
                    , gif: Gif }

-- actions

type Action = MousePos (Int, Int)
  | DragStart (Int, Int)
  | DragEnd
  | Tick Time

-- animation duration

duration = 300 * millisecond

-- init models from json

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2 Model decodeAnimationModel decodeGif

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object7 AnimationModel (Json.succeed False)
                              (Json.succeed (0 * millisecond))
                              (Json.succeed (0 * millisecond))
                              (Json.succeed 0)
                              (Json.succeed 0)
                              (Json.succeed 0)
                              (Json.succeed 0)

decodeGif: Json.Decoder Gif
decodeGif =
  Json.object3 Gif (Json.at ["images", "fixed_width", "url"] Json.string)
                  (Json.at ["images", "fixed_width", "width"] Json.string)
                  (Json.at ["images", "fixed_width", "height"] Json.string)

-- update

update: Action -> Model -> (Model, Effects Action)
update action model =
  let { x, y, dx, dy, isClicked, elapsedTime, prevClockTime } = model.animationState
  in
    case action of
      MousePos (a, b) ->
        if isClicked then
          let dx = (a - x)
              dy = (b - y)
          in
            ({ model | animationState = (AnimationModel isClicked elapsedTime prevClockTime x y dx dy) }, Effects.none)
        else
          (model, Effects.none)

      DragStart (a, b) ->
          ({ model | animationState = (AnimationModel True 0 0 a b dx dy) }, Effects.none)

      DragEnd ->
        ({ model | animationState = (AnimationModel False elapsedTime prevClockTime x y dx dy) }, Effects.tick Tick)

      Tick clockTime ->
        let newElapsedTime = if prevClockTime == 0 then
              0
            else
              elapsedTime + (clockTime - prevClockTime)
            oldAnimationState = model.animationState
            newAnimationState = if newElapsedTime > duration then
              { oldAnimationState | elapsedTime = 0, prevClockTime = 0, dx = 0, dy = 0 }
            else
              { oldAnimationState | elapsedTime = newElapsedTime, prevClockTime = clockTime }
            effects = if newElapsedTime > duration then
              Effects.none
            else
              Effects.tick Tick
        in
          ({ model | animationState = newAnimationState }, effects)

animate : Time -> Float -> Float -> Float
animate currentTime start end =
  ease easeOutBounce float start end duration currentTime

view: Signal.Address Action ->  Bool -> Model -> Html
view address draggable model =
  div (getGifAttributes ( model , draggable ) address) []

decoder =
  Json.object2 (,) ("pageX" := Json.int) ("pageY" := Json.int)

translate3d x y =
  ("transform", "translate(" ++ x ++ "px, " ++ y ++ "px)")

getGifAttributes: ( Model , Bool ) -> Signal.Address Action -> List (Attribute)
getGifAttributes ( model , draggable ) address =
  if draggable then
    [ Html.Events.on "mousedown" decoder (\val -> Signal.message address (DragStart val))
    , onMouseUp address DragEnd
    , style (getStyle model draggable) ]
  else
    [ style (getStyle model draggable) ]


getStyle: Model ->  Bool -> List ((String, String))
getStyle model draggable =
  let { width, height } = model.gif
      { dx, dy, isClicked, elapsedTime } = Debug.log "lol" model.animationState
      transform = if isClicked
        then translate3d (toString dx) (toString dy)
        else translate3d (toString (animate elapsedTime (Basics.toFloat dx) 0))
                        (toString (animate elapsedTime (Basics.toFloat dy) 0))
      position = if draggable
        then [ ( "position", "relative" ), ( "z-index", "100") ]
        else [ ( "position", "absolute" ), ("transform", "translate(5%, -95%)") ]
  in
    transform ::
    List.append position [ ("width", model.gif.width ++ "px")
    , ("height", model.gif.height ++ "px")
    , ("backgroundImage", "url(" ++ model.gif.url ++ ")")
    , ("cursor", "pointer")
    , ("border-radius", "3px")
    ]
