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
import ElmFire
import Gif
import Easing exposing (..)
import Time exposing (Time, millisecond)

type Animation
  = MouseDragging
  | DragBack
  | FadeOut
  | Swipe
  | None

type alias AnimationModel =
  { animationType: Animation
  , elapsedTime: Time
  , prevClockTime: Time
  , startPos: ( Float, Float )
  , relativeStartPos: ( Float, Float )
  , endPos: ( Float, Float ) }

type alias Model =
  { animationState: AnimationModel
  , gif: Gif.Model }

-- actions

type Action
  = DragStart ( Float, Float, Float, Float )
  | DragEnd ( Float, Float )
  | AnimationTick Time
  | SwipeRight
  | SwipeLeft
  | NoOp (Maybe ElmFire.Reference)

-- animation duration

duration =
  300 * millisecond

-- init models from json

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2
    Model
    decodeAnimationModel
    Gif.decodeGifFromGiphy

decodeAnimationModel: Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object6
    AnimationModel
    (Json.succeed None)
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

hasBeenSwiped: ( Int, Int ) -> ( Float, Float ) -> Bool
hasBeenSwiped ( windowWidth, windowHeight ) ( dx, dy ) =
  (abs dx) > (toFloat (windowWidth // 4))

-- update

update: Action -> Model -> { b | window : ( Int, Int ) } -> ( ( Model, Int ), Effects Action )
update action model global =
  let { startPos, endPos, animationType
      , elapsedTime, prevClockTime
      , relativeStartPos } = model.animationState
      { animationState } = model
  in
    case action of
      DragStart (x, y, a, b) ->
          ( ( { model | animationState = (AnimationModel MouseDragging 0 0 ( x, y ) ( a, b ) endPos) }, 0 ), Effects.none )

      DragEnd newEndPos ->
        let next = hasBeenSwiped global.window newEndPos
        in
          if next then
            let newAnimationState = { animationState | animationType = FadeOut, endPos = newEndPos}
            in
              ( ( { model | animationState = newAnimationState }, 0 ), Effects.tick AnimationTick )
          else
            let newAnimationState = { animationState | animationType = DragBack, endPos = newEndPos}
            in
              ( ( { model | animationState = newAnimationState }, 0 ), Effects.tick AnimationTick )

      AnimationTick clockTime ->
        let newElapsedTime = calculateElapsedTime
                              clockTime
                              prevClockTime
                              elapsedTime
        in
          if newElapsedTime > duration then
            let newAnimationState = { animationState | animationType = None, elapsedTime = 0, prevClockTime = 0 }
                next = hasBeenSwiped global.window endPos
                action = if next && (fst endPos) < 0 then -1
                  else if next && (fst endPos) > 0 then 1
                  else 0
            in
              ( ( { model | animationState = newAnimationState }, action ), Effects.none )
          else
            let newAnimationState = { animationState | elapsedTime = newElapsedTime, prevClockTime = clockTime }
            in
              ( ( { model | animationState = newAnimationState }, 0 ), Effects.tick AnimationTick )

      SwipeRight ->
        ( ( { model | animationState = { animationState | animationType = Swipe,
                                                          startPos = ( 0, 0 ),
                                                          relativeStartPos = ( 0, 0 ),
                                                          endPos = ( (toFloat (fst global.window)), 0 ) } }, 0 ), Effects.tick AnimationTick )

      SwipeLeft ->
        ( ( { model | animationState = { animationState | animationType = Swipe,
                                                          startPos = ( 0, 0 ),
                                                          relativeStartPos = ( 0, 0 ),
                                                          endPos = ( -1 * (toFloat (fst global.window)), 0 ) } }, 0 ), Effects.tick AnimationTick )

      NoOp ref -> ( ( model, 0 ), Effects.none )

easeBack: Time -> Float -> Float -> Float
easeBack currentTime start end =
  ease easeOutBounce float start end duration currentTime

easeOpacity : Time -> Float -> Float -> Float
easeOpacity currentTime start end =
  ease easeOutExpo float start end duration currentTime

getDelta: AnimationModel -> ( Int, Int ) -> ( Float, Float )
getDelta animationModel mousePos =
  let { startPos, endPos, animationType, elapsedTime } = animationModel
      ( mouseX, mouseY ) = mousePos
      ( startX, startY ) = startPos
      ( endPosX, endPosY ) = endPos
  in
    case animationType of
      MouseDragging ->
        ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )
      DragBack ->
        let
          (dx, dy) = ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )
        in
          ( (easeBack elapsedTime endPosX 0), (easeBack elapsedTime endPosY 0) )
      Swipe ->
        ( (easeBack elapsedTime 0 (fst endPos)), startY )
      FadeOut ->
         endPos
      None ->
        ( 0, 0 )

view: Signal.Address Action -> Bool -> { a | mouse : ( Int, Int ) } -> Int -> Model -> Html
view address isFirstOfStack global index model =
  let { startPos, endPos, animationType, elapsedTime } = model.animationState
      delta = getDelta model.animationState global.mouse
      cardAttributes = getCardAttributes model
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
  Json.object2 (,) ("pageX" := Json.float) ("pageY" := Json.float)

relativeDecoder =
  Json.object4 (,,,) ("pageX" := Json.float) ("pageY" := Json.float) ("offsetX" := Json.float) ("offsetY" := Json.float)

getCardAttributes:  Model ->  Bool  -> ( Float, Float ) -> Signal.Address Action -> Int -> List (Attribute)
getCardAttributes model isFirstOfStack delta address index =
  if isFirstOfStack then
    [ Html.Events.on "mousedown" relativeDecoder (\val -> Signal.message address (DragStart val))
    , Html.Events.on "mouseup" decoder (\val -> Signal.message address (DragEnd delta))
    , style (getCardStyle model isFirstOfStack delta index) ]
  else
    [ style (getCardStyle model isFirstOfStack delta index) ]

getCardStyle: Model ->  Bool  -> ( Float, Float ) -> Int -> List (( String, String ))
getCardStyle model isFirstOfStack ( dx, dy ) index =
  let { elapsedTime, animationType, relativeStartPos } = model.animationState
      ( relX, relY ) = relativeStartPos
      height  = model.gif.height
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200
      offsetX = toString 0
      offsetY = toString (13 + height - (3 * (1 + index)))
      gifOpacity = if animationType == FadeOut
        then toString (easeOpacity elapsedTime 1 0)
        else "1"
      transform = translateAndRotate dx dy relX relY
      position = if isFirstOfStack
        then [ ( "position", "relative" ), ( "z-index", "100") ]
        else [ ( "position", "absolute" ), ( "transform", "translate3d(" ++ offsetX ++ "px, -" ++ offsetY ++ "px, 0px)" ) ]
  in
    ("opacity", gifOpacity) :: List.concat [ transform, position ]

translateAndRotate: Float ->  Float ->  Float ->  Float -> List ( String, String )
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
