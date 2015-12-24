module App (Model, init, Action, update, view) where

import Html exposing (..)
import Html.Events exposing (onClick)
import ElmFire exposing (..)
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import TaskTutorial exposing (print)
import Task
import ElmFire
import Graphics.Element exposing (..)
import Json.Decode exposing (..)

type alias Model = {
  root: ElmFire.Location,
  user: Maybe ( Authentication )
}

init: String -> (Model, Effects Action)
init init = ({root = ElmFire.fromUrl init
  , user = Nothing },
   Effects.none)

type Action = LoginRequest
  | Login (Maybe Authentication)
  | Logout

login: ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthPopup "facebook")
    |> Task.toMaybe
    |> Task.map Login
    |> Effects.task

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    LoginRequest ->
      (model
      , login model.root)

    Login auth ->
      ( { model | user = auth }
      , Effects.none )

    Logout ->
      ( model
      , Effects.none )

view: Signal.Address Action -> Model -> Html
view address model =
  let name = case model.user of
      Just user ->
        (Result.withDefault "lol" (decodeValue getDisplayName user.specifics))

      Nothing ->
         "Not loggedin"
  in
    div []
      [button [onClick address LoginRequest] [text "Login"]
      , div [] [text name]]



getDisplayName : Decoder String
getDisplayName =
    "displayName" := string
