import ElmFire.Auth exposing (..)
import ElmFire exposing (ErrorType)
import Graphics.Element exposing (..)
import Task exposing (..)
import TaskTutorial exposing (print)
import Effects exposing (Never)
import App exposing (..)
import MyStartApp as StartApp
import Gif
import GifContainer
import Mouse

app =
  StartApp.start
    { init = init "https://gipher.firebaseio.com"
    , update = update
    , view = view
    , inputs = [Signal.map mousePos Mouse.position]
    }

mousePos: (Int, Int) -> App.Action
mousePos pos =
  (App.GifContainer (GifContainer.Gif (Gif.MousePos pos)))

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
