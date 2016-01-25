module StackCard where

import Json.Decode as Json exposing ((:=))
import Json.Encode
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
import ElmFire
import Gif
import Easing exposing (..)
import Time exposing (Time, millisecond)

type alias AnimationModel =
  { isClicked: Bool
  , opacityElapsedTime: Time
  , elapsedTime: Time
  , prevClockTime: Time
  , startPos: ( Int, Int )
  , relativeStartPos: ( Int, Int )
  , endPos: ( Int, Int ) }

type alias Model =
  { animationState: AnimationModel
  , gif: Gif.Model }

-- actions

type Action
  = DragStart ( Int, Int, Int, Int )
  | DragEnd ( Int, Int )
  | DragTick Time
  | FadeTick Time
  | NoOp (Maybe ElmFire.Reference)

-- animation duration

duration =
  200 * millisecond

-- init models from json

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2
    Model
    decodeAnimationModel
    Gif.decodeGifFromGiphy

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object7
    AnimationModel
    (Json.succeed False)
    (Json.succeed (0 * millisecond))
    (Json.succeed (0 * millisecond))
    (Json.succeed (0 * millisecond))
    (Json.succeed (0, 0))
    (Json.succeed (0, 0))
    (Json.succeed (0, 0))

calculateElapsedTime: Time -> Time -> Time -> Time
calculateElapsedTime clockTime prevClockTime elapsedTime  =
  if prevClockTime == 0 then
    0
  else
    elapsedTime + (clockTime - prevClockTime)

-- update

update: Action -> Model -> Global.Model -> ( ( Model, Int ), Effects Action )
update action model global =
  let { startPos, endPos, isClicked
      , elapsedTime, prevClockTime
      , opacityElapsedTime, relativeStartPos } = model.animationState
  in
    case action of
      DragStart (x, y, a, b) ->
          ( ( { model | animationState = (AnimationModel True 0 0 0 ( x, y ) ( a, b ) endPos) }, 0 ), Effects.none)

      DragEnd newEndPos ->
        let (dx, dy) = newEndPos
            next = (abs dx) > ((fst global.window) // 4)
            animationState = model.animationState
            newAnimationState = { animationState | isClicked = False, endPos = newEndPos}
        in
          if next then
            (({ model | animationState = newAnimationState }, 0), Effects.tick FadeTick)
          else
            (({ model | animationState = newAnimationState }, 0), Effects.tick DragTick)

      FadeTick clockTime ->
        let newElapsedTime = calculateElapsedTime
                              clockTime
                              prevClockTime
                              opacityElapsedTime
            oldAnimationState = model.animationState
        in
          if newElapsedTime > duration then
            let newAnimationState = { oldAnimationState | opacityElapsedTime = 0, prevClockTime = 0 }
                result = if (fst endPos) > 0 then 1 else -1
            in
              ( ( { model | animationState = newAnimationState }, result ), Effects.none )
          else
            let newAnimationState = { oldAnimationState | opacityElapsedTime = newElapsedTime, prevClockTime = clockTime }
            in
              ( ( { model | animationState = newAnimationState }, 0 ), Effects.tick FadeTick )

      DragTick clockTime ->
        let newElapsedTime = calculateElapsedTime
                              clockTime
                              prevClockTime
                              elapsedTime
            oldAnimationState = model.animationState
            newAnimationState = if newElapsedTime > duration
              then
                { oldAnimationState | elapsedTime = 0, prevClockTime = 0, endPos = ( 0, 0 ) }
              else
                { oldAnimationState | elapsedTime = newElapsedTime, prevClockTime = clockTime }
            effects = if newElapsedTime > duration then
              Effects.none
            else
              Effects.tick DragTick
        in
          ( ( { model | animationState = newAnimationState }, 0 ), effects )

      NoOp ref -> ( ( model, 0 ), Effects.none )

easeBack: Time -> Float -> Float -> Float
easeBack currentTime start end =
  ease easeOutBounce float start end duration currentTime

easeOpacity : Time -> Float -> Float -> Float
easeOpacity currentTime start end =
  ease easeOutExpo float start end duration currentTime

view: Signal.Address Action -> Bool -> Global.Model -> Int -> Model -> Html
view address isFirstOfStack global index model =
  let { startPos, endPos, isClicked, elapsedTime } = model.animationState
      ( startX, startY ) = startPos
      ( mouseX, mouseY ) = global.mouse
      delta = if isClicked then ( mouseX - startX, mouseY - startY ) else endPos
      cardAttributes = getCardAttributes
                         model
                         isFirstOfStack
                         delta
                         address
                         index
      dx = fst delta
      tag = if (abs dx) < 10 then
          div [] []
        else
          tagElement (dx > 0)
  in
    div cardAttributes
        [ Gif.cardView model.gif , tag ]

tagElement: Bool -> Html
tagElement liked =
  let label = if liked then "LIKED" else "NOPE"
      color = if liked then "#00FF95" else "#FF2300"
  in
    div
      [ style [ ( "position", "absolute" )
              , ( "top", "25px" )
              , ( "transform", "rotateZ(-25deg)" )
              , ( "left", "20px" )
              , ( "font-size", "30px" )
              , ( "color", color )
              , ( "border", "3px solid" )
              , ( "border-radius", "10px" )
              , ( "padding", "5px" )
              , ( "opacity", "0.8") ] ]
      [ text label ]

decoder =
  Json.object2 (,) ("pageX" := Json.int) ("pageY" := Json.int)

relativeDecoder =
  Json.object4 (,,,) ("pageX" := Json.int) ("pageY" := Json.int) ("offsetX" := Json.int) ("offsetY" := Json.int)

getCardAttributes:  Model ->  Bool  -> ( Int, Int ) -> Signal.Address Action -> Int -> List (Attribute)
getCardAttributes model isFirstOfStack delta address index =
  if isFirstOfStack then
    [ Html.Events.on "mousedown" relativeDecoder (\val -> Signal.message address (DragStart val))
    , Html.Events.on "mouseup" decoder (\val -> Signal.message address (DragEnd delta))
    , style (getCardStyle model isFirstOfStack delta index) ]
  else
    [ style (getCardStyle model isFirstOfStack delta index) ]

getCardStyle: Model ->  Bool  -> ( Int, Int ) -> Int -> List (( String, String ))
getCardStyle model isFirstOfStack ( dx, dy ) index =
  let { elapsedTime, isClicked, opacityElapsedTime, relativeStartPos } = model.animationState
      ( relX, relY ) = relativeStartPos
      height  = model.gif.height
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200
      offsetX = toString (3 * (1 + index))
      offsetY = toString (13 + height + (3 * (1 + index)))
      gifOpacity = ("opacity", (toString (easeOpacity opacityElapsedTime 1 0)))
      transform = if isClicked
        then translateAndRotate (toFloat dx) (toFloat dy) relX relY
        else translateAndRotate (easeBack elapsedTime (Basics.toFloat dx) 0) (easeBack elapsedTime (Basics.toFloat dy) 0) relX relY
      position = if isFirstOfStack
        then [ ( "position", "relative" ), ( "z-index", "100") ]
        else [ ( "position", "absolute" ), ( "transform", "translate3d(" ++ offsetX ++ "px, -" ++ offsetY ++ "px, 0px)" ) ]
  in
    gifOpacity :: List.concat [ transform, position ]

translateAndRotate dx dy relX relY =
  let limit = 100
      coefX = if dx > limit then limit
        else if dx < -limit then -limit
        else dx
      coefY = if dy > limit then limit
        else if dy < -limit then -limit
        else dy
  in
  [ ("transform", "translate3d(" ++ (toString dx) ++ "px, " ++ (toString dy) ++ "px, 0px) rotate3d(0,0,1," ++ (toString (0.002 * coefX * coefY)) ++ "deg)")
  , ("transform-origin", (toString relX) ++ "px " ++ (toString relY) ++ "px") ]
