module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseUp)
import ElmFire exposing (..)
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Task
import ElmFire
import Graphics.Element exposing (..)
import Json.Decode exposing (..)
import GifContainer
import Gif
import Global exposing ( User )
import Html.Attributes exposing (style)

 -- Model

type alias Model =
  { global: Global.Model
  , gif: GifContainer.Model
  }

init: String -> (Model, Effects Action)
init init =
  let (gifModel, gifEffect) = GifContainer.init
  in
    ({ global = { root = ElmFire.fromUrl init
                , user = Nothing
                , mouse = ( 0, 0 )
                , window = ( 0, 0) }
     , gif = gifModel
     }, (Effects.map GifContainer gifEffect))

  -- Actions

type Action = Login (Maybe Authentication)
  | Logout
  | GifContainer GifContainer.Action
  | MousePos ( Int, Int )
  | Resize ( Int, Int )

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
            global = model.global
            newGlobal = { global | user = userObject }
          in
            ( { model | global = newGlobal }
            , Effects.none )

        Nothing ->
          ( model, login model.global.root )

    Logout ->
      ( model
      , Effects.none )

    GifContainer gifAction ->
      let (model1, effects) = GifContainer.update gifAction model.gif model.global
      in
        ({model | gif = model1}, (Effects.map GifContainer effects))

    MousePos pos ->
    let global = model.global
        newGlobal = { global | mouse = pos }
    in
      ({ model | global = newGlobal }, Effects.none )

    Resize size ->
          let global = model.global
              newGlobal = { global | window = size }
          in
            ({ model | global = newGlobal }, Effects.none )

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let
    test = Debug.log "global" model.global
    body = case model.global.user of
      Just user -> GifContainer.view (Signal.forwardTo address GifContainer) model.gif model.global
      Nothing -> loginView address model
  in
    div  [containerStyle]
      (icons :: font :: css "gipher.css" :: body :: [])

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
  node "link" [ href "https://fonts.googleapis.com/css?family=Source+Sans+Pro", rel "stylesheet" ] [ ]

icons: Html
icons =
  node "link" [ href "https://fonts.googleapis.com/icon?family=Material+Icons", rel "stylesheet" ] [ ]

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
