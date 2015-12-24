import ElmFire.Auth exposing (..)
import ElmFire exposing (ErrorType)
import Graphics.Element exposing (..)
import Task exposing (..)
import TaskTutorial exposing (print)
import Effects exposing (Never)
import App exposing (init, update, view)
import StartApp


app =
  StartApp.start
    { init = init "https://gipher.firebaseio.com"
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
