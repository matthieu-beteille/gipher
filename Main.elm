import ElmFire.Auth exposing (..)
import ElmFire exposing (ErrorType)
import Json.Encode exposing (encode)
import Json.Decode
import Graphics.Element exposing (..)
import Task exposing (..)
import Effects exposing (Never, Effects)
import App exposing (..)
import ElmFire
import StartApp
import StackCard exposing ( mailBox )
import Stack
import Mouse
import Window
import LikedGifs
import Gif

port title: String
port title = "Gipher"

app =
  let (model, effects) = init False
  in
    StartApp.start
      { init = (model, Effects.batch [effects, sendInitial])
      , update = (update firebaseMailbox.address)
      , view = view
      , inputs = [ resizes
                 , firstResize
                 , firebaseSignal
                 , Signal.map (\mouse -> App.Stack (Stack.StackCard (StackCard.Drag mouse))) StackCard.draggingSignal ] }

main =
  app.html

port tasks: Signal (Task.Task Never ())
port tasks =
  app.tasks

firebaseMailbox: Signal.Mailbox Json.Encode.Value
firebaseMailbox =
  Signal.mailbox Json.Encode.null

firebaseSignal: Signal Action
firebaseSignal =
  Signal.map
    ( \response ->
        let gif = Json.Decode.decodeValue Gif.decodeGifFromFirebase response
                  |> Result.toMaybe
        in
          case gif of
            Just value ->  App.LikedGifs (LikedGifs.Data value)
            Nothing -> App.NoOp )
    firebaseMailbox.signal

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
