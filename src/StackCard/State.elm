module StackCard.State (..) where

import StackCard.Types exposing (..)
import StackCard.Inputs exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Signal
import Time exposing (Time, millisecond)

-- animation duration

duration : Time
duration =
  300 * millisecond


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
