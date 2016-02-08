module StackCard (..) where

import Json.Decode as Json exposing ((:=))
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Signal
import Mouse
import Gif
import Easing exposing (..)
import Time exposing (Time, millisecond)
import Signal.Extra exposing (keepWhen)


draggingMailbox : Signal.Mailbox Bool
draggingMailbox =
  Signal.mailbox False


draggingSignal : Signal ( Int, Int )
draggingSignal =
  keepWhen draggingMailbox.signal ( 0, 0 ) Mouse.position


nextGifMailbox : Signal.Mailbox Bool
nextGifMailbox =
  Signal.mailbox False


type Animation
  = MouseDragging
  | DragBack
  | FadeOut
  | Swipe
  | None


type alias AnimationModel =
  { animationType : Animation
  , elapsedTime : Time
  , prevClockTime : Time
  , startPos : ( Float, Float )
  , relativeStartPos : ( Float, Float )
  , endPos : ( Float, Float )
  , mouse : ( Int, Int )
  }


type alias Model =
  { animationState : AnimationModel
  , gif : Gif.Model
  }



-- actions


type Action
  = DragStart ( ( Float, Float ), ( Float, Float ) )
  | Drag ( Int, Int )
  | DragEnd ( Float, Float )
  | AnimationTick Time
  | SwipeRight
  | SwipeLeft
  | NoOp



-- animation duration


duration : Time
duration =
  300 * millisecond



-- init models from json


decodeModel : Json.Decoder Model
decodeModel =
  Json.object2
    Model
    decodeAnimationModel
    Gif.decodeGifFromGiphy


decodeAnimationModel : Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object7
    AnimationModel
    (Json.succeed None)
    (Json.succeed (0 * millisecond))
    (Json.succeed (0 * millisecond))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))



-- helpers


calculateElapsedTime : Time -> Time -> Time -> Time
calculateElapsedTime clockTime prevClockTime elapsedTime =
  if prevClockTime == 0 then
    0
  else
    elapsedTime + (clockTime - prevClockTime)


hasBeenSwiped : ( Int, Int ) -> ( Float, Float ) -> Bool
hasBeenSwiped ( windowWidth, windowHeight ) ( dx, dy ) =
  (abs dx) > (toFloat (windowWidth // 4))



-- update


update : Action -> Model -> { b | window : ( Int, Int ) } -> ( Model, Effects Action )
update action model global =
  let
    { startPos, endPos, animationType, elapsedTime, prevClockTime, relativeStartPos, mouse } =
      model.animationState

    { animationState } =
      model
  in
    case action of
      DragStart ( pos, relativePos ) ->
        let
          newAnimationState =
            { animationState | startPos = pos, relativeStartPos = relativePos }

          startDrag =
            Signal.send draggingMailbox.address True
              |> Task.toMaybe
              |> Task.map (\_ -> NoOp)
              |> Effects.task
        in
          ( ({ model | animationState = newAnimationState }), startDrag )

      Drag newMouse ->
        let
          newAnimationState =
            { animationState | animationType = MouseDragging, mouse = newMouse }
        in
          ( ({ model | animationState = newAnimationState }), Effects.none )

      DragEnd newEndPos ->
        let
          next =
            hasBeenSwiped global.window newEndPos

          endDrag =
            Signal.send draggingMailbox.address False
              |> Task.toMaybe
              |> Task.map (\_ -> NoOp)
              |> Effects.task

          effects =
            Effects.batch [ endDrag, Effects.tick AnimationTick ]
        in
          if next then
            let
              newAnimationState =
                { animationState | animationType = FadeOut, endPos = newEndPos }
            in
              ( ({ model | animationState = newAnimationState }), effects )
          else
            let
              newAnimationState =
                { animationState | animationType = DragBack, endPos = newEndPos }
            in
              ( ({ model | animationState = newAnimationState }), effects )

      AnimationTick clockTime ->
        let
          newElapsedTime =
            calculateElapsedTime
              clockTime
              prevClockTime
              elapsedTime
        in
          if newElapsedTime > duration then
            let
              newAnimationState =
                { animationState | animationType = None, elapsedTime = 0, prevClockTime = 0 }

              next =
                hasBeenSwiped global.window endPos

              endEffects =
                if next && (fst endPos) < 0 then
                  Signal.send nextGifMailbox.address False
                    |> Task.toMaybe
                    |> Task.map (\_ -> NoOp)
                    |> Effects.task
                else if next && (fst endPos) > 0 then
                  Signal.send nextGifMailbox.address True
                    |> Task.toMaybe
                    |> Task.map (\_ -> NoOp)
                    |> Effects.task
                else
                  Effects.none
            in
              ( ({ model | animationState = newAnimationState }), endEffects )
          else
            let
              newAnimationState =
                { animationState | elapsedTime = newElapsedTime, prevClockTime = clockTime }
            in
              ( ({ model | animationState = newAnimationState }), Effects.tick AnimationTick )

      SwipeRight ->
        ( ({ model
            | animationState =
                { animationState
                  | animationType = Swipe
                  , startPos = ( 0, 0 )
                  , relativeStartPos = ( 0, 0 )
                  , endPos = ( (toFloat (fst global.window)), 0 )
                }
           }
          )
        , Effects.tick AnimationTick
        )

      SwipeLeft ->
        ( ({ model
            | animationState =
                { animationState
                  | animationType = Swipe
                  , startPos = ( 0, 0 )
                  , relativeStartPos = ( 0, 0 )
                  , endPos = ( -1 * (toFloat (fst global.window)), 0 )
                }
           }
          )
        , Effects.tick AnimationTick
        )

      NoOp ->
        ( model, Effects.none )



-- easing


easeBack : Time -> Float -> Float -> Float
easeBack currentTime start end =
  ease easeOutBounce float start end duration currentTime


easeOpacity : Time -> Float -> Float -> Float
easeOpacity currentTime start end =
  ease easeOutExpo float start end duration currentTime



-- delta calculation


getDelta : AnimationModel -> ( Int, Int ) -> ( Float, Float )
getDelta animationModel mousePos =
  let
    { startPos, endPos, animationType, elapsedTime } =
      animationModel

    ( mouseX, mouseY ) =
      mousePos

    ( startX, startY ) =
      startPos

    ( endPosX, endPosY ) =
      endPos
  in
    case animationType of
      MouseDragging ->
        ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )

      DragBack ->
        let
          ( dx, dy ) =
            ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )
        in
          ( (easeBack elapsedTime endPosX 0), (easeBack elapsedTime endPosY 0) )

      Swipe ->
        ( (easeBack elapsedTime 0 (fst endPos)), startY )

      FadeOut ->
        endPos

      None ->
        ( 0, 0 )



-- view


view : Signal.Address Action -> Model -> Html
view address model =
  let
    { startPos, endPos, animationType, elapsedTime, mouse } =
      model.animationState

    delta =
      getDelta model.animationState mouse

    cardAttributes =
      getCardAttributes
        model
        delta
        address

    dx =
      fst delta

    tag =
      if (abs dx) < 10 then
        div [] []
      else
        tagElement (dx > 0)
  in
    div (getContainerAttributes delta address) [ div cardAttributes [ Gif.cardView model.gif, tag ] ]


tagElement : Bool -> Html
tagElement liked =
  let
    label =
      if liked then
        "LIKED"
      else
        "NOPE"

    color =
      if liked then
        "#00FF95"
      else
        "#FF2300"
  in
    div
      [ style
          [ ( "position", "absolute" )
          , ( "top", "25px" )
          , ( "transform", "rotateZ(-25deg)" )
          , ( "left", "20px" )
          , ( "font-size", "30px" )
          , ( "color", color )
          , ( "border", "3px solid" )
          , ( "border-radius", "10px" )
          , ( "padding", "5px" )
          , ( "opacity", "0.8" )
          ]
      ]
      [ text label ]



-- event decoders


decoder : Json.Decoder ( Float, Float )
decoder =
  Json.object2 (,) ("pageX" := Json.float) ("pageY" := Json.float)


doubleTuple : Float -> Float -> Float -> Float -> ( ( Float, Float ), ( Float, Float ) )
doubleTuple a b c d =
  ( ( a, b ), ( c, d ) )


relativeDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ) )
relativeDecoder =
  Json.object4
    doubleTuple
    ("pageX" := Json.float)
    ("pageY" := Json.float)
    ("offsetX" := Json.float)
    ("offsetY" := Json.float)



-- decoderTouchMove =
--   Json.object1 identity ("touches" := Json.list decoder)


getContainerAttributes : ( Float, Float ) -> Signal.Address Action -> List (Attribute)
getContainerAttributes delta address =
  [ Html.Events.on "mouseup" decoder (\val -> Signal.message address (DragEnd delta))
  , style [ ( "padding", "40px" ) ]
  ]


getCardAttributes : Model -> ( Float, Float ) -> Signal.Address Action -> List (Attribute)
getCardAttributes model delta address =
  let mouseDownListener = if model.animationState.animationType /= None then
      Html.Events.on "mousedown" relativeDecoder (\val -> Signal.message address NoOp)
    else
      Html.Events.on "mousedown" relativeDecoder (\val -> Signal.message address (DragStart val))
  in
    [ mouseDownListener
    , style (getCardStyle model delta)
    ]


getCardStyle : Model -> ( Float, Float ) -> List ( String, String )
getCardStyle model ( dx, dy ) =
  let
    { elapsedTime, animationType, relativeStartPos } =
      model.animationState

    ( relX, relY ) =
      relativeStartPos

    gifOpacity =
      if animationType == FadeOut then
        toString (easeOpacity elapsedTime 1 0)
      else
        "1"

    transform =
      translateAndRotate dx dy relX relY
  in
    ( "opacity", gifOpacity )
      :: ( "z-index", "100" )
      :: ( "position", "relative" )
      :: List.concat [ transform ]


translateAndRotate : Float -> Float -> Float -> Float -> List ( String, String )
translateAndRotate dx dy relX relY =
  let
    limit =
      100

    coefX =
      if dx > limit then
        limit
      else if dx < -limit then
        -limit
      else
        dx

    coefY =
      if dy > limit then
        limit
      else if dy < -limit then
        -limit
      else
        dy
  in
    [ ( "transform", "translate3d(" ++ (toString dx) ++ "px, " ++ (toString dy) ++ "px, 0px) rotate3d(0,0,1," ++ (toString (2.0e-3 * coefX * coefY)) ++ "deg)" )
    , ( "transform-origin", (toString relX) ++ "px " ++ (toString relY) ++ "px" )
    ]
