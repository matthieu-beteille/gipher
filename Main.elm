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
import Window

app =
  StartApp.start
    { init = init "https://gipher.firebaseio.com"
    , update = update
    , view = view
    , inputs = [ Signal.map App.MousePos Mouse.position 
               , Signal.map App.Resize Window.dimensions ]
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
