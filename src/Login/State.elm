module Login.State (..) where

import Login.Types exposing (..)
import Login.Inputs exposing (loginBox)
import Login.Rest exposing (getUserFromAuth)
import LikedGifs.Inputs exposing (firebaseMailbox)
import Signup.State
import ElmFire exposing (childAdded, noOrder)
import ElmFire.Auth exposing (..)
import Task
import Effects exposing (..)
import Effects


init : ElmFire.Location -> ( Model, Effects Action )
init loc =
  let
    effects =
      subscribeAuth (\auth -> Signal.send loginBox.address auth) loc
        |> Task.toMaybe
        |> Task.map (\_ -> NoOp)
        |> Effects.task
  in
    ( { user = Nothing, signup = Signup.State.init }, effects )


login : ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthRedirect "facebook")
    |> Task.toMaybe
    |> Task.map (always NoOp)
    |> Effects.task


update : Action -> Model -> ElmFire.Location -> ( Model, Effects Action )
update action model root =
  case action of
    LoginRequest ->
      ( model, login root )

    Login auth ->
      case model.user of
        Nothing ->
          let
            user =
              (getUserFromAuth auth)

            userObject =
              Just user

            effects =
              case user.subscription of
                Nothing ->
                  ElmFire.subscribe
                    (Signal.send firebaseMailbox.address << .value)
                    (always (Task.succeed ()))
                    (childAdded noOrder)
                    (ElmFire.sub ("likedGifs/" ++ user.uid) root)
                    |> Task.toMaybe
                    |> Task.map Subscribed
                    |> Effects.task

                Just sub ->
                  Effects.none
          in
            ( { model | user = userObject }, effects )

        Just user ->
          ( model, Effects.none )

    Logout ->
      let
        effects =
          case model.user of
            Just user ->
              case user.subscription of
                Just sub ->
                  let
                    unsubscribeEffect =
                      ElmFire.unsubscribe sub
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task

                    logoutEffect =
                      ElmFire.Auth.unauthenticate root
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task
                  in
                    Effects.batch [ unsubscribeEffect, logoutEffect ]

                Nothing ->
                  Effects.none

            Nothing ->
              Effects.none
      in
        ( { model | user = Nothing }, effects )

    NoOp ->
      ( model, Effects.none )

    Subscribed sub ->
      case model.user of
        Just user ->
          ( { model | user = Just { user | subscription = sub } }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    Signup signupAction ->
      let
        ( newSignup, effects ) =
          Signup.State.update signupAction model.signup root
      in
        ( { model | signup = newSignup }, Effects.map Signup effects )
