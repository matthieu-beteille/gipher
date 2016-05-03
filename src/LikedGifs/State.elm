module LikedGifs.State (..) where

import LikedGifs.Types exposing (..)
import Effects exposing (Effects)
import Stack.Types
import Task


init : Model
init =
  []


update : Action -> Model -> ( Model, Effects Stack.Types.Action )
update action model =
  case action of
    -- we receive each liked gif one by one from firebase
    -- each time we receive one we remove it from the stack (Stack.Remove)
    Data gif ->
      let
        effects =
          Effects.task <| Task.succeed (Stack.Types.Remove gif)
      in
        ( gif :: model, effects )
