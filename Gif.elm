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
import Basics exposing (abs)
import Global
import Easing exposing (..)
import Time exposing (Time, millisecond)

type alias Gif =
    { url: String
    , width: String
    , height: String }

type alias AnimationModel =
    { isClicked: Bool
      , opacityElapsedTime: Time
      , elapsedTime: Time
      , prevClockTime: Time
      , startPos: ( Int, Int )
      , endPos: ( Int, Int )
      }


type alias Model =
    { animationState: AnimationModel
    , gif: Gif }

-- actions

type Action
  = DragStart (Int, Int)
  | DragEnd (Int, Int)
  | DragTick Time
  | FadeTick Time

-- animation duration

duration =
  200 * millisecond

-- init models from json

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2 Model decodeAnimationModel decodeGif

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object6 AnimationModel (Json.succeed False)
                              (Json.succeed (0 * millisecond))
                              (Json.succeed (0 * millisecond))
                              (Json.succeed (0 * millisecond))
                              (Json.succeed (0, 0))
                              (Json.succeed (0, 0))

decodeGif: Json.Decoder Gif
decodeGif =
  Json.object3 Gif (Json.at ["images", "fixed_height", "url"] Json.string)
                  (Json.at ["images", "fixed_height", "width"] Json.string)
                  (Json.at ["images", "fixed_height", "height"] Json.string)

-- update

update: Action -> Model -> Global.Model -> (( Model, Bool ), Effects Action)
update action model global =
  let { startPos, endPos, isClicked, elapsedTime, prevClockTime, opacityElapsedTime } = model.animationState
  in
    case action of
      DragStart newStartPos ->
          (({ model | animationState = (AnimationModel True 0 0 0 newStartPos endPos) }, False), Effects.none)

      DragEnd newEndPos ->
        let (dx, dy) = newEndPos
            next = (abs dx) > ((fst global.window) // 4)
            animationState = model.animationState
            newAnimationState = { animationState | isClicked = False, endPos = newEndPos}
        in
          if next then
            (({ model | animationState = newAnimationState }, False), Effects.tick FadeTick)
          else
            (({ model | animationState = newAnimationState }, False), Effects.tick DragTick)

      FadeTick clockTime ->
        let newElapsedTime = if prevClockTime == 0 then 0 else opacityElapsedTime + (clockTime - prevClockTime)
            oldAnimationState = model.animationState
            newAnimationState = if newElapsedTime > duration then
              { oldAnimationState | opacityElapsedTime = 0, prevClockTime = 0}
            else
              { oldAnimationState | opacityElapsedTime = newElapsedTime, prevClockTime = clockTime }
            effects = if newElapsedTime > duration then
              Effects.none
            else
              Effects.tick FadeTick
        in
          if newElapsedTime > duration then
            (({ model | animationState = newAnimationState }, True), effects)
          else
            (({ model | animationState = newAnimationState }, False), effects)

      DragTick clockTime ->
        let newElapsedTime = if prevClockTime == 0 then 0 else elapsedTime + (clockTime - prevClockTime)
            oldAnimationState = model.animationState
            newAnimationState = if newElapsedTime > duration then
              { oldAnimationState | elapsedTime = 0, prevClockTime = 0, endPos = ( 0, 0 )}
            else
              { oldAnimationState | elapsedTime = newElapsedTime, prevClockTime = clockTime }
            effects = if newElapsedTime > duration then
              Effects.none
            else
              Effects.tick DragTick
        in
          (({ model | animationState = newAnimationState }, False), effects)

animate : Time -> Float -> Float -> Float
animate currentTime start end =
  ease easeOutBounce float start end duration currentTime

animateOpacity : Time -> Float -> Float -> Float
animateOpacity currentTime start end =
  ease easeOutExpo float start end duration currentTime

view: Signal.Address Action -> Bool -> Global.Model -> Int -> Model -> Html
view address draggable global index model =
  div (getCardAttributes ( model, draggable ) global  address index)
    [ div [ getImgStyle model ] [] ]

getImgStyle: Model -> Attribute
getImgStyle model =
  let { width, height, url } = model.gif
  in
    style [ ("width", "200px")
          , ("height", height ++ "px")
          , ("backgroundImage", "url(" ++ url ++ ")")
          , ("backgroundPosition", "center center")
          , ("border-radius", "3px") ]

decoder =
  Json.object2 (,) ("pageX" := Json.int) ("pageY" := Json.int)

translate3d x y =
  ("transform", "translate(" ++ (toString x) ++ "px, " ++ (toString y) ++ "px)")

getCardAttributes: ( Model , Bool ) -> Global.Model -> Signal.Address Action -> Int -> List (Attribute)
getCardAttributes ( model , draggable ) global address index =
  let { startPos, endPos, isClicked, elapsedTime } = model.animationState
      (startX, startY ) = startPos
      ( mouseX, mouseY ) = global.mouse
      delta = if isClicked then (mouseX - startX, mouseY - startY) else endPos
  in
    if draggable then
      [ Html.Events.on "mousedown" decoder (\val -> Signal.message address (DragStart val))
      , Html.Events.on "mouseup" decoder (\val -> Signal.message address (DragEnd delta))
      , Html.Events.on "touchstart" decoder (\val -> Signal.message address (DragStart val))
      , Html.Events.on "touchend" decoder (\val -> Signal.message address (DragEnd delta))
      , style (getCardStyle model draggable delta index) ]
    else
      [ style (getCardStyle model draggable delta index) ]

getCardStyle: Model ->  Bool  -> ( Int, Int ) -> Int -> List ((String, String))
getCardStyle model draggable ( dx, dy ) index =
  let { elapsedTime, isClicked, opacityElapsedTime } = model.animationState
      height  = model.gif.height
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200
      offsetX = toString (3 * (1 + index))
      offsetY = toString (13 + height + (3 * (1 +index)))
      gifOpacity = ("opacity", (toString (animateOpacity opacityElapsedTime 1 0)))
      transform = if isClicked
        then translate3d dx dy
        else translate3d ((animate elapsedTime (Basics.toFloat dx) 0))
                          (animate elapsedTime (Basics.toFloat dy) 0)
      position = if draggable
        then [ ( "position", "relative" ), ( "z-index", "100") ]
        else [ ( "position", "absolute" ), ("transform", "translate(" ++ offsetX ++ "px, -" ++ offsetY ++ "px)") ]
  in
    gifOpacity :: transform :: List.append position [ ("border", "1px solid #BBBFBE")
                                                    , ("overflow-x", "hidden")
                                                    , ("padding", "5px")
                                                    , ("background-color", "white")
                                                    , ("cursor", "pointer")
                                                    , ("border-radius", "3px")
                                                    ]
