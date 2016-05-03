module Main (..) where

import Task exposing (..)
import Effects exposing (Never, Effects)
import App.State exposing (..)
import App.Types exposing (..)
import App.View exposing (view)
import StartApp
import StackCard exposing (nextGifMailbox)
import Stack
import Window
import LikedGifs exposing (firebaseSignal)
import Html
import Login exposing (loginSignal)

app : StartApp.App Model
app =
  let
    ( model, effects ) =
      init
  in
    StartApp.start
      { init = ( model, Effects.batch [ effects, sendInitial ] )
      , update = update
      , view = view
      , inputs =
          [ resizes
          , firstResize
          , Signal.map (\action -> App.Types.Login action) loginSignal
          , Signal.map (\hasBeenLiked -> App.Types.Stack (Stack.NextCard hasBeenLiked)) nextGifMailbox.signal
          , Signal.map (\action -> App.Types.LikedGifs action) firebaseSignal
          , Signal.map
              (\mouse -> App.Types.Stack (Stack.StackCard (StackCard.Drag mouse)))
              StackCard.draggingSignal
          , Signal.map (always App.Types.NoOp) swap
          ]
      }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- HOT-SWAPPING
port swap : Signal.Signal Bool

-- signal to get the initial window size


resizes : Signal Action
resizes =
  Signal.map App.Types.Resize Window.dimensions


appStartMailbox : Signal.Mailbox ()
appStartMailbox =
  Signal.mailbox ()


firstResize : Signal Action
firstResize =
  Signal.sampleOn appStartMailbox.signal resizes


sendInitial : Effects Action
sendInitial =
  Signal.send appStartMailbox.address ()
    -- Task a ()
    |>
      Task.map (always App.Types.NoOp)
    |>
      Effects.task
