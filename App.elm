module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ElmFire exposing (..)
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Task
import ElmFire
import Graphics.Element exposing (..)
import Json.Decode exposing (..)
import GifContainer
import Html.Attributes exposing (style)

 -- Model

type alias User =
  { uid: String
  , token: String
  , displayName: String
  }

type alias Model =
  { root: ElmFire.Location
  , user: Maybe ( User )
  , gif: GifContainer.Model
  }

init: String -> (Model, Effects Action)
init init =
  let (gifModel, gifEffect) = GifContainer.init
  in
    ({ root = ElmFire.fromUrl init
     , user = Nothing
     , gif = gifModel
     }, (Effects.map GifContainer gifEffect))

  -- Actions

type Action = Login (Maybe Authentication)
  | Logout
  | GifContainer GifContainer.Action

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

    GifContainer gifAction ->
      let (model1, effects) = GifContainer.update gifAction model.gif
      in
        ({model | gif = model1}, (Effects.map GifContainer effects))

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let body = case model.user of
    Just user -> GifContainer.view (Signal.forwardTo address GifContainer) model.gif
    Nothing -> loginView address model
  in
    div [containerStyle]
      (font :: css "gipher.css" :: body :: [])

loginView: Signal.Address Action -> Model -> Html
loginView address model =
  div []
  [h1 [titleStyle] [text "Gipher"]
  , div [btnStyle] [a [onClick address (Login Nothing)] [text "Login with Facebook"]]]


css: String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

font: Html
font =
  node "link" [ href "https://fonts.googleapis.com/css?family=Source+Sans+Pro", rel "stylesheet" ] []

containerStyle: Attribute
containerStyle =
  style [
    ("font-family", "Source Sans Pro")
  , ("background-color", "#3b5998")
  , ("height", "100%")
  , ("display", "flex")
  , ("justify-content", "center")
  , ("align-items", "center")
  ]

titleStyle: Attribute
titleStyle =
  style [
    ("color", "white")
  , ("text-align", "center")
  , ("margin-bottom", "50px")
  , ("font-size", "2.5em")
  ]

btnStyle : Attribute
btnStyle =
  style
    [ ("font-size", "20px")
    , ("color", "white")
    , ("cursor", "pointer")
    , ("display", "inline-block")
    , ("width", "100px")
    , ("text-align", "center")
    , ("border", "1px solid white")
    , ("border-radius", "3px")
    , ("padding", "10px")
    ]
