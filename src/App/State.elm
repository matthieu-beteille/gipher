module App.State (..) where

import App.Types exposing (..)
import Effects exposing (..)
import ElmFire exposing (Snapshot, childAdded, noOrder, noLimit)
import Stack
import LikedGifs
import Login


firebaseUrl : String
firebaseUrl =
  "https://gipher.firebaseio.com"


-- Init


init : ( Model, Effects Action )
init =
  let
    ( loginModel, loginEffect ) =
      Login.init loc

    ( gifModel, gifEffect ) =
      Stack.init

    loc =
      ElmFire.fromUrl firebaseUrl

    effects =
      Effects.batch [ Effects.map Stack gifEffect, Effects.map Login loginEffect ]
  in
    ( { global =
          { root = loc
          , login = loginModel
          , window = ( 0, 0 )
          , isMenuOpened = False
          , route = Home
          }
      , newGifs = gifModel
      , likedGifs = []
      }
    , effects
    )



-- Update


update : Action -> Model -> ( Model, Effects Action )
update action model =
  let
    { global } =
      model
  in
    case action of
      Login loginAction ->
        let
          ( newLogin, effects ) =
            Login.update loginAction model.global.login model.global.root

          newGlobal =
            { global | login = newLogin }
        in
          if newLogin.user == Nothing then
            let
              newGlobal =
                { newGlobal | isMenuOpened = False, isMenuOpened = False, route = Home }

              ( newGifs, gifEffect ) =
                Stack.init

              logoutEffects =
                Effects.batch [ Effects.map Stack gifEffect, Effects.map Login effects ]
            in
              ( { model | global = newGlobal, newGifs = newGifs, likedGifs = [] }, logoutEffects )
          else
            ( { model | global = newGlobal }, (Effects.map Login effects) )

      Stack stackAction ->
        let
          ( newModel, effects ) =
            Stack.update
              stackAction
              model.newGifs
              model.likedGifs
              model.global
        in
          ( { model | newGifs = newModel }, (Effects.map Stack effects) )

      Resize size ->
        let
          newGlobal =
            { global | window = size }
        in
          ( { model | global = newGlobal }, Effects.none )

      LikedGifs action ->
        let
          ( newLikedGifs, effects ) =
            LikedGifs.update action model.likedGifs
        in
          ( { model | likedGifs = newLikedGifs }, Effects.map Stack effects )

      ToggleMenu ->
        let
          newMenu =
            not model.global.isMenuOpened

          newGlobal =
            { global | isMenuOpened = newMenu }
        in
          ( { model | global = newGlobal }, Effects.none )

      GoTo route ->
        let
          newGlobal =
            { global | route = route, isMenuOpened = False }
        in
          ( { model | global = newGlobal }, Effects.none )

      NoOp ->
        ( model, Effects.none )
