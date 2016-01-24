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
import Easing exposing (..)
import Time exposing (Time, millisecond)

type alias Gif =
  { id: String
  , url: String
  , width: String
  , height: String
  , smallWidth: String
  , smallHeight: String }

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
  , gif: Gif }

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

encodeGif : Gif -> Json.Encode.Value
encodeGif gif =
  Json.Encode.object
    [ ( "url", Json.Encode.string gif.url )
    , ( "width", Json.Encode.string gif.width )
    , ( "height", Json.Encode.string gif.height )
    , ( "smallWidth", Json.Encode.string gif.smallWidth )
    , ( "smallHeight", Json.Encode.string gif.smallHeight )
    , ( "id", Json.Encode.string gif.id ) ]

decodeModel: Json.Decoder Model
decodeModel =
  Json.object2
    Model
    decodeAnimationModel
    decodeGifFromGiphy

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

decodeGifFromGiphy: Json.Decoder Gif
decodeGifFromGiphy =
  Json.object6
    Gif
    (Json.at ["id"] Json.string)
    (Json.at ["images", "fixed_height", "url"] Json.string)
    (Json.at ["images", "fixed_height", "width"] Json.string)
    (Json.at ["images", "fixed_height", "height"] Json.string)
    (Json.at ["images", "fixed_height_small", "width"] Json.string)
    (Json.at ["images", "fixed_height_small", "height"] Json.string)

decodeGifFromFirebase: Json.Decoder Gif
decodeGifFromFirebase =
  Json.object6
    Gif (Json.at ["id"] Json.string)
    (Json.at ["url"] Json.string)
    (Json.at ["width"] Json.string)
    (Json.at ["height"] Json.string)
    (Json.at ["width"] Json.string)
    (Json.at ["height"] Json.string)


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
        [ div [ getImgStyle model ] [] , tag ]

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

getImgStyle: Model -> Attribute
getImgStyle model =
  let { width, height, url } = model.gif
  in
    style [ ( "width", "200px" )
          , ( "height", height ++ "px" )
          , ( "backgroundImage", "url(" ++ url ++ ")" )
          , ( "backgroundPosition", "center center" )
          , ( "border-radius", "3px" ) ]

decoder =
  Json.object2 (,) ("pageX" := Json.int) ("pageY" := Json.int)

relativeDecoder =
  Json.object4 (,,,) ("pageX" := Json.int) ("pageY" := Json.int) ("offsetX" := Json.int) ("offsetY" := Json.int)

translateAndRotate dx dy relX relY =
  let limit = 100
      coefX = if dx > limit then limit
        else if dx < -limit then -limit
        else dx
      coefY = if dy > limit then limit
        else if dy < -limit then -limit
        else dy
  in
  [ ("transform", "translate(" ++ (toString dx) ++ "px, " ++ (toString dy) ++ "px) rotate(" ++ (toString (0.002 * coefX * coefY)) ++ "deg)")
  , ("transform-origin", (toString relX) ++ "px " ++ (toString relY) ++ "px") ]

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
        else [ ( "position", "absolute" ), ( "transform", "translate(" ++ offsetX ++ "px, -" ++ offsetY ++ "px)" ) ]
  in
    gifOpacity :: List.concat [ transform, position, [ ("border", "1px solid #BBBFBE")
                                                      , ("overflow-x", "hidden")
                                                      , ("padding", "5px")
                                                      , ("background-color", "white")
                                                      , ("cursor", "pointer")
                                                      , ("border-radius", "3px") ] ]
