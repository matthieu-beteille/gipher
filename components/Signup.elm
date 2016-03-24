module Signup (..) where

import ElmFire
import ElmFire.Auth exposing (..)
import Effects exposing (..)
import Json.Decode exposing (..)
import Effects
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Debug
import Task

type alias Model =
  { username : String
  , password : String
  , error : String
  }


init : Model
init =
  Model "" "" ""


type Action
  = TypeUsername String
  | TypePassword String
  | Signup
  | Success (Maybe String)
  | Error
  | NoOp


handleSignup : Maybe (Maybe String) -> Action
handleSignup result =
  case result of
    Just action ->
      Success action

    Nothing ->
      NoOp


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
      let test = authenticate loc [] (withPassword model.username model.password)
            |> Task.toMaybe
            |> Task.map (always NoOp)
            |> Effects.task
      in
        ( model, test )


    Error ->
      ( { model | error = "Signup failed (login must be an email address)" }, Effects.none )

    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Html
view address =
  div
    []
    [ span [] [ text "Username" ]
    , input
        [ type' "text"
        , on "input" targetValue (\value -> Signal.message address (TypeUsername value))
        ]
        []
    , span [] [ text "Password" ]
    , input
        [ type' "password"
        , on "input" targetValue (\value -> Signal.message address (TypePassword value))
        ]
        []
    , div
        [ btnStyle, class "login-btn", onClick address Signup ]
        [ text "Sign Up" ]
    ]


btnStyle : Attribute
btnStyle =
  style
    [ ( "font-size", "20px" )
    , ( "cursor", "pointer" )
    , ( "display", "inline-block" )
    , ( "width", "200px" )
    , ( "text-align", "center" )
    , ( "border", "1px solid white" )
    , ( "border-radius", "3px" )
    , ( "padding", "10px" )
    , ( "letter-spacing", "-1px" )
    ]
