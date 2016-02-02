import Task exposing (..)
import Effects exposing (Never, Effects)
import App exposing (..)
import StartApp
import StackCard exposing ( mailBox )
import Stack
import Window
import LikedGifs exposing ( firebaseSignal )
import Html

port title: String
port title = "Gipher"

app: StartApp.App Model
app =
  let (model, effects) = init False
  in
    StartApp.start
      { init = (model, Effects.batch [effects, sendInitial])
      , update = update
      , view = view
      , inputs = [ resizes
                 , firstResize
                 , Signal.map (\gif -> App.NewLikedGif gif) firebaseSignal
                 , Signal.map (\mouse -> App.Stack (Stack.StackCard (StackCard.Drag mouse)))
                              StackCard.draggingSignal ] }

main: Signal Html.Html
main =
  app.html

port tasks: Signal (Task.Task Never ())
port tasks =
  app.tasks

-- signal to get the initial window size

resizes: Signal Action
resizes =
    Signal.map App.Resize Window.dimensions

appStartMailbox: Signal.Mailbox ()
appStartMailbox =
    Signal.mailbox ()

firstResize: Signal Action
firstResize =
  Signal.sampleOn appStartMailbox.signal resizes

sendInitial: Effects Action
sendInitial =
  Signal.send appStartMailbox.address () -- Task a ()
      |> Task.map (always App.NoOp)
      |> Effects.task
