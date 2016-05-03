module StackCard.Types (..) where

import Gif.Types
import Time exposing (Time, millisecond)


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
  , gif : Gif.Types.Model
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
