module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onMouseUp )
import ElmFire exposing (..)
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Task
import ElmFire exposing ( Snapshot, childAdded, noOrder, noLimit )
import Graphics.Element exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Stack
import StackCard
import Global
import Login
import Html.Attributes exposing ( style )
import Gif

 -- Model

type alias Model =
  { global: Global.Model
  , newGifs: Stack.Model
  , likedGifs: List ( Gif.Model )
  }

 -- init

init: String -> (Model, Effects Action)
init init =
  let ( gifModel, gifEffect ) = Stack.init
  in
    ( { global = { root = ElmFire.fromUrl init
                 , user = Login.init
                 , mouse = ( 0, 0 )
                 , window = ( 0, 0 ) }

     , newGifs = gifModel

     , likedGifs = [] }, (Effects.map Stack gifEffect) )

  -- Actions

type Action
  = Stack Stack.Action
  | Login Login.Action
  | MousePos ( Int, Int )
  | Resize ( Int, Int )
  | Data Gif.Model
  | NoOp

  -- Update

update: Signal.Address Json.Encode.Value -> Action -> Model -> (Model, Effects Action)
update address action model =
  case action of
    Login loginAction ->
      let ( newUser , effects ) = Login.update address loginAction model.global.user model.global.root
          global = model.global
          newGlobal = { global | user = newUser }
      in
        ( { model | global = newGlobal }, (Effects.map Login effects) )

    Stack stackAction ->
      let ( newModel, effects ) = Stack.update
                                    stackAction
                                    model.newGifs
                                    model.global
      in
        ( { model | newGifs = newModel }, (Effects.map Stack effects) )

    MousePos pos ->
    let global = model.global
        newGlobal = { global | mouse = pos }
    in
      ( { model | global = newGlobal }, Effects.none )

    Resize size ->
          let global = model.global
              newGlobal = { global | window = size }
          in
            ( { model | global = newGlobal }, Effects.none )

    NoOp ->
      ( model, Effects.none )

    Data gif ->
      let newLikedGifs = (Debug.log "received") gif :: model.likedGifs
      in
        ( { model | likedGifs = newLikedGifs } , Effects.none )

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let
    body = case model.global.user of
      Just user ->
        Stack.view (Signal.forwardTo address Stack) model.newGifs model.global
      Nothing ->
        Login.loginView (Signal.forwardTo address Login) model.global.user
  in
    div [ containerStyle ] (icons :: font :: css "gipher.css" :: body :: [])


css: String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

font: Html
font =
  node "link" [ href "https://fonts.googleapis.com/css?family=Source+Sans+Pro"
              , rel "stylesheet" ] []

icons: Html
icons =
  node "link" [ href "https://fonts.googleapis.com/icon?family=Material+Icons"
              , rel "stylesheet" ] []

containerStyle: Attribute
containerStyle =
  style [ ("overflow", "hidden")
        , ("font-family", "Source Sans Pro")
        , ("background-color", "#3b5998")
        , ("height", "100%")
        , ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center") ]
