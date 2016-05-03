module Signup.State (..) where

import Signup.Types exposing (..)
import ElmFire
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Task

init : Model
init =
  Model "" "" "" ""

handleSignup : Maybe (Maybe String) -> Action
handleSignup result =
  case result of
    Just action ->
      Success action

    Nothing ->
      Error


update : Action -> Model -> ElmFire.Location -> ( Model, Effects Action )
update action model loc =
  case action of
    TypeUsername newUsername ->
      ( { model | username = newUsername }, Effects.none )

    TypePassword newPassword ->
      ( { model | password = newPassword }, Effects.none )

    Signup ->
      let
        test =
          userOperation loc (createUser model.username model.password)
            |> Task.toMaybe
            |> Task.map handleSignup
            |> Effects.task
      in
        ( model, test )

    Success a ->
      let
        effect =
          authenticate loc [] (withPassword model.username model.password)
            |> Task.toMaybe
            |> Task.map (always NoOp)
            |> Effects.task
      in
        ( { model | success = "Sign-up successful, signing you in now...." } , effect )

    Login ->
      let
        effect =
          authenticate loc [] (withPassword model.username model.password)
            |> Task.toMaybe
            |> Task.map LoginResult
            |> Effects.task
      in
        ( model, effect )

    Error ->
      ( { model | error = "Signup failed (username should be a valid email address)" }, Effects.none )

    NoOp ->
      ( model, Effects.none )

    LoginResult result ->
      case result of
        Just result ->
          ( model, Effects.none )

        Nothing ->
          ( { model | error = "Username or password incorrect" }, Effects.none )
