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
import Global exposing ( User )
import Html.Attributes exposing ( style )
import Gif

 -- Model

type alias Model =
  { global: Global.Model
  , newGifs: Stack.Model
  , likedGifs: List ( Gif.Model )
  }

init: String -> (Model, Effects Action)
init init =
  let ( gifModel, gifEffect ) = Stack.init
  in
    ( { global = { root = ElmFire.fromUrl init
                 , user = Nothing
                 , mouse = ( 0, 0 )
                 , window = ( 0, 0 ) }

     , newGifs = gifModel

     , likedGifs = [] }, (Effects.map Stack gifEffect) )

  -- Actions

type Action
  = Login (Maybe Authentication)
  | Logout
  | Stack Stack.Action
  | MousePos ( Int, Int )
  | Resize ( Int, Int )
  | Data Gif.Model
  | NoOp

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

update: Signal.Address Json.Encode.Value -> Action -> Model -> (Model, Effects Action)
update address action model =
  case action of
    Login auth ->
      case auth of
        Just auth ->
          let user = (getUserFromAuth auth)
              userObject = Just user
              global = model.global
              newGlobal = { global | user = userObject }
              effects = ElmFire.subscribe
                          (Signal.send address << .value)
                          (always (Task.succeed ()))
                          (childAdded noOrder)
                          (ElmFire.sub user.uid model.global.root)
                            |> Task.toMaybe
                            |> Task.map (always NoOp)
                            |> Effects.task
          in
            ( { model | global = newGlobal }, effects )

        Nothing ->
          ( model, login model.global.root )

    Logout ->
      ( model, Effects.none )

    Stack containerAction ->
      let ( newModel, effects ) = Stack.update
                                    containerAction
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
      let newLikedGifs = gif :: model.likedGifs
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
        loginView address model
  in
    div [ containerStyle ] (icons :: font :: css "gipher.css" :: body :: [])

loginView: Signal.Address Action -> Model -> Html
loginView address model =
  div [] [ h1 [titleStyle] [text "Gipher"]
          , div [btnStyle] [ a [onClick address (Login Nothing)] [text "Login with Facebook"] ] ]


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

titleStyle: Attribute
titleStyle =
  style [ ("color", "white")
        , ("text-align", "center")
        , ("margin-bottom", "50px")
        , ("font-size", "2.5em") ]

btnStyle : Attribute
btnStyle =
  style [ ("font-size", "20px")
        , ("color", "white")
        , ("cursor", "pointer")
        , ("display", "inline-block")
        , ("width", "100px")
        , ("text-align", "center")
        , ("border", "1px solid white")
        , ("border-radius", "3px")
        , ("padding", "10px") ]
