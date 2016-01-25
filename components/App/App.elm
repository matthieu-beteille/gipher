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
import LikedGifs
import Login
import Html.Attributes exposing ( style )
import Gif

 -- App Routes

type Route
  = Home
  | MyGifs

 -- Model

type alias Model =
  { global: { root: ElmFire.Location
            , user: Login.Model
            , mouse: ( Int, Int )
            , window: ( Int, Int )
            , isMenuOpened: Bool
            , route: Route }
  , newGifs: Stack.Model
  , likedGifs: LikedGifs.Model
  }

 -- Init

init: String -> (Model, Effects Action)
init init =
  let ( gifModel, gifEffect ) = Stack.init
  in
    ( { global = { root = ElmFire.fromUrl init
                 , user = Login.init
                 , mouse = ( 0, 0 )
                 , window = ( 0, 0 )
                 , isMenuOpened = False
                 , route = Home }

     , newGifs = gifModel

     , likedGifs = [] }, (Effects.map Stack gifEffect) )

  -- Actions

type Action
  = Stack Stack.Action
  | Login Login.Action
  | LikedGifs LikedGifs.Action
  | MousePos ( Int, Int )
  | Resize ( Int, Int )
  | ToggleMenu
  | GoTo Route
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

    LikedGifs action ->
      let newLikedGifs = LikedGifs.update action model.likedGifs
      in
        ( { model | likedGifs = newLikedGifs }, Effects.none )

    ToggleMenu ->
      let newMenu = not model.global.isMenuOpened
          oldGlobal = model.global
          newGlobal = { oldGlobal | isMenuOpened = newMenu }
      in
        ( { model | global = newGlobal }, Effects.none )

    GoTo route ->
      let oldGlobal = model.global
          newGlobal = { oldGlobal | route = route }
      in
        ( { model | global = newGlobal }, Effects.none )

    NoOp ->
      ( model, Effects.none )

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let
    body = case model.global.user of
      Just user ->
        [ navBar address,
          overlayMenu address model.global.isMenuOpened,
          Stack.view (Signal.forwardTo address Stack) model.newGifs model.global ]
      Nothing ->
        [ Login.loginView (Signal.forwardTo address Login) model.global.user ]
  in
    div [ containerStyle ] (icons :: font :: css "gipher.css" :: body )

navBar address =
  div [ onClick address ToggleMenu, navbarStyle ] [ i [class "material-icons hover", hamburgerStyle] [text "menu"] ]

overlayMenu address isOpened =
    div [ overlayStyle isOpened ] [ div [ class "hover", menuItemStyle ] [text "Home"]
                                   , div [ class "hover", menuItemStyle, onClick address (GoTo Home)] [text "Liked Gifs"]
                                   , div [ class "hover", menuItemStyle, onClick address (Login Login.Logout) ] [ text "Logout" ]
                                   , i [ class "material-icons hover", crossStyle, (onClick address ToggleMenu) ]
                                       [text "clear"] ]

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
  style [ ( "overflow", "hidden" )
        , ( "font-family", "Source Sans Pro" )
        , ( "background-color", "#0076E5" )
        , ( "height", "100%" )
        , ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" ) ]

overlayStyle isOpened =
  let translateValue = if isOpened then "0%" else "150%"
      opacity = if isOpened then "0.97" else "0"
      transition = "transform 10ms ease 100ms, opacity 250ms cubic-bezier(.165,.84,.44,1)"
  in
    style [  ( "position", "fixed" )
           , ( "transition", transition )
           , ( "transform", "translate3d(0," ++ translateValue ++ ", 0)" )
           , ( "background-color", "#00FF95" )
           , ( "z-index", "101" )
           , ( "top", "0" )
           , ( "left", "0" )
           , ( "width", "100%" )
           , ( "height", "100%" )
           , ( "opacity", opacity ) ]

menuItemStyle =
  style [ ( "position", "relative" )
        , ( "top", "100px" )
        , ( "left", "50px" )
        , ( "line-height", "5pc" )
        , ( "font-size", "40px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]

hamburgerStyle =
  style [ ( "font-size", "40px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]

navbarStyle =
  style [ ( "position", "fixed" )
        , ( "top", "20px" )
        , ( "left", "30px" )
        , ( "z-index", "102" ) ]

crossStyle =
  style [ ( "position", "absolute" )
        , ( "top", "20px" )
        , ( "right", "30px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]
