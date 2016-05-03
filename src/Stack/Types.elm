module Stack.Types (..) where

import StackCard.Types
import Gif.Types

type alias Model =
  List StackCard.Types.Model

type Action
  = NewGifs (Maybe Model)
  | StackCard StackCard.Types.Action
  | NextCard Bool
  | Remove Gif.Types.Model
  | NoOp
