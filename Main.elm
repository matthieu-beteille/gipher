import ElmFire.Auth exposing (..)
import ElmFire exposing (ErrorType)
import Json.Encode exposing (encode)
import Json.Decode
import Graphics.Element exposing (..)
import Task exposing (..)
import TaskTutorial exposing (print)
import Effects exposing (Never, Effects)
import App exposing (..)
import ElmFire
import StartApp
import Gif
import Container
import Mouse
import Window

responses : Signal.Mailbox Json.Encode.Value
responses = Signal.mailbox Json.Encode.null

app =
  let (model, effects) = init "https://gipher.firebaseio.com"
  in
    StartApp.start
      { init = (model, Effects.batch [sendInitial, effects])
      , update = (update responses.address)
      , view = view
      , inputs = [ Signal.map App.MousePos Mouse.position
                 , resizes
                 , firstResize
                 , signal ] }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


signal: Signal Action
signal = Signal.map
        ( \response ->
            let gif = Json.Decode.decodeValue Gif.decodeGifFromFirebase response
                      |> Result.toMaybe
            in
              case gif of
                Just value ->  App.Data value
                Nothing -> App.NoOp )
        responses.signal

-- to get the initial window size

resizes : Signal Action
resizes =
    Signal.map App.Resize Window.dimensions

appStartMailbox : Signal.Mailbox ()
appStartMailbox =
    Signal.mailbox ()

firstResize: Signal Action
firstResize = Signal.sampleOn appStartMailbox.signal resizes

sendInitial : Effects Action
sendInitial =
    Signal.send appStartMailbox.address () -- Task a ()
        |> Task.map (always App.NoOp)
        |> Effects.task
