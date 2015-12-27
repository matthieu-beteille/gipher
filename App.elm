module App (Model, init, Action, update, view) where

import Html exposing (..)
import Html.Events exposing (onClick)
import ElmFire exposing (..)
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Task
import ElmFire
import Graphics.Element exposing (..)
import Json.Decode exposing (..)
import Gif
 -- Model

type alias User =
  { uid: String
  , token: String
  , displayName: String
  }

type alias Model =
  { root: ElmFire.Location
  , user: Maybe ( User )
  , gif: Gif.Model
  }

init: String -> (Model, Effects Action)
init init =
  let (gifModel, gifEffect) = Gif.init
  in
    ({ root = ElmFire.fromUrl init
     , user = Nothing
     , gif = gifModel
     }, (Effects.map NewGif gifEffect))

  -- Actions

type Action = Login (Maybe Authentication)
  | Logout
  | NewGif Gif.Action

  -- Update

login: ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthPopup "facebook")
    |> Task.toMaybe
    |> Task.map Login
    |> Effects.task

decodeDisplayName: Decoder String
decodeDisplayName =
    "displayName" := string

getUserFromAuth: Authentication -> User
getUserFromAuth auth =
  User auth.uid auth.token (Result.withDefault "" (decodeValue decodeDisplayName auth.specifics))


update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Login auth ->
      case auth of
        Just auth ->
          let
            userObject = Just (getUserFromAuth auth)
          in
            ( { model | user = userObject }
            , Effects.none )

        Nothing ->
          (model, login model.root)

    Logout ->
      ( model
      , Effects.none )

    NewGif gifAction ->
      let (model1, effects) = Gif.update gifAction model.gif
      in
        ({model | gif = model1}, (Effects.map NewGif effects))

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let body = case model.user of
    Just user -> Gif.view (Signal.forwardTo address NewGif) model.gif
    Nothing -> button [onClick address (Login Nothing)] [text "Login"]
  in
    div []
      (body :: [])
