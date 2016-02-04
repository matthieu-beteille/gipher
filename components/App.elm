module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onMouseUp )
import ElmFire exposing (..)
import Effects exposing (..)
import ElmFire exposing ( Snapshot, childAdded, noOrder, noLimit )
import Stack
import LikedGifs
import Login

firebaseUrl: String
firebaseUrl =
  "https://gipher.firebaseio.com"

 -- App Routes

type Route
  = Home
  | MyGifs

 -- Model

type alias Model =
  { global: { root: ElmFire.Location
            , user: Login.Model
            , window: ( Int, Int )
            , isMenuOpened: Bool
            , route: Route }
  , newGifs: Stack.Model
  , likedGifs: LikedGifs.Model
  }

 -- Init

init: (Model, Effects Action)
init =
  let ( userModel, loginEffect ) = Login.init loc
      ( gifModel, gifEffect ) = Stack.init
      loc = ElmFire.fromUrl firebaseUrl
      effects = Effects.batch [ Effects.map Stack gifEffect, Effects.map Login loginEffect ]
  in
    ( { global = { root = loc
                 , user = userModel
                 , window = ( 0, 0 )
                 , isMenuOpened = False
                 , route = Home }

     , newGifs = gifModel

     , likedGifs = [] }, effects )

  -- Actions

type Action
  = Stack Stack.Action
  | Login Login.Action
  | LikedGifs LikedGifs.Action
  | Resize ( Int, Int )
  | ToggleMenu
  | GoTo Route
  | NoOp

  -- Update

update: Action -> Model -> (Model, Effects Action)
update action model =
  let { global } = model
  in
    case action of
      Login loginAction ->
        let ( newUser , effects ) = Login.update loginAction model.global.user model.global.root
            newGlobal = { global | user = newUser }
        in
          if newUser == Nothing then
            let
              newGlobal = { newGlobal | isMenuOpened = False, isMenuOpened = False, route = Home }
              ( newGifs, gifEffect ) = Stack.init
              logoutEffects = Effects.batch [ Effects.map Stack gifEffect, Effects.map Login effects ]
            in
              ( { model | global = newGlobal, newGifs = newGifs, likedGifs = [] }, logoutEffects )
          else
            ( { model | global = newGlobal }, (Effects.map Login effects) )

      Stack stackAction ->
        let ( newModel, effects ) = Stack.update
                                      stackAction
                                      model.newGifs
                                      model.likedGifs
                                      model.global
        in
          ( { model | newGifs = newModel }, (Effects.map Stack effects) )

      Resize size ->
            let newGlobal = { global | window = size }
            in
              ( { model | global = newGlobal }, Effects.none )

      LikedGifs action ->
        let ( newLikedGifs, effects ) = LikedGifs.update action model.likedGifs
        in
          ( { model | likedGifs = newLikedGifs }, Effects.map Stack effects )

      ToggleMenu ->
        let newMenu = not model.global.isMenuOpened
            newGlobal = { global | isMenuOpened = newMenu }
        in
          ( { model | global = newGlobal }, Effects.none )

      GoTo route ->
        let newGlobal = { global | route = route, isMenuOpened = False }
        in
          ( { model | global = newGlobal }, Effects.none )

      NoOp ->
          ( model, Effects.none )

  -- View

view: Signal.Address Action -> Model -> Html
view address model =
  let overflowY = model.global.route == MyGifs
      view = case model.global.route of
        Home -> Stack.view (Signal.forwardTo address Stack) model.newGifs model.global
        MyGifs -> LikedGifs.view model.likedGifs
      body = case model.global.user of
        Just user ->
          [ navBar address model.global.isMenuOpened,
            overlayMenu address model.global.isMenuOpened,
            view ]
        Nothing ->
          [ Login.loginView (Signal.forwardTo address Login) model.global.user ]
  in
    div [ containerStyle overflowY ] ( head :: githubLink :: body )

githubLink: Html
githubLink =
  a [ href "https://github.com/matthieu-beteille/gipher"
    , target "_blank"
    , style [ ( "position", "absolute" )
            , ( "right", "20px" )
            , ( "bottom", "10px" ) ] ]
    [ img [ src "https://cdn0.iconfinder.com/data/icons/octicons/1024/mark-github-128.png"
          , style [ ( "width", "30px" ) ] ]
          [ ] ]


navBar: Signal.Address Action -> Bool -> Html
navBar address isMenuOpened =
  let closeMenuIcon = if isMenuOpened
    then i [ class "material-icons hover", crossStyle, (onClick address ToggleMenu) ] [ text "clear" ]
    else div [] []
  in
    div [ navbarStyle ] [ div [ onClick address ToggleMenu, class "material-icons hover", hamburgerStyle ] [ text "menu" ]
                        , div [ navbarTitleStyle ] [ text "Gipher" ]
                        , closeMenuIcon ]

overlayMenu: Signal.Address Action -> Bool -> Html
overlayMenu address isOpened =
    div [ overlayStyle isOpened ] [ div [ class "hover", menuItemStyle, onClick address (GoTo Home) ] [text "Home"]
                                   , div [ class "hover", menuItemStyle, onClick address (GoTo MyGifs)] [text "Liked Gifs"]
                                   , div [ class "hover", menuItemStyle, onClick address (Login Login.Logout) ] [ text "Logout" ] ]

head: Html
head =
  node "head" [] [ css "gipher.css"
                 , meta
                 , font
                 , icons ]

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

meta: Html
meta =
  node "meta" [ name "viewport"
              , content "width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimal-ui" ] []

containerStyle: Bool -> Attribute
containerStyle overflowY =
  let overflow = if overflowY then "auto" else "hidden"
  in
    style [ ( "overflow-y", overflow )
          , ( "overflow-x", "hidden" )
          , ( "font-family", "Source Sans Pro" )
          , ( "background-color", "#0076E5" )
          , ( "height", "100%" ) ]

overlayStyle: Bool -> Attribute
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

menuItemStyle: Attribute
menuItemStyle =
  style [ ( "position", "relative" )
        , ( "top", "100px" )
        , ( "left", "50px" )
        , ( "line-height", "5pc" )
        , ( "font-size", "40px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]

hamburgerStyle: Attribute
hamburgerStyle =
  style [ ( "font-size", "40px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" )
        , ( "position", "absolute")
        , ( "left", "30px" ) ]

navbarStyle: Attribute
navbarStyle =
  style [ ( "position", "fixed" )
        , ( "display", "flex" )
        , ( "width", "100%" )
        , ( "align-item", "center" )
        , ( "padding-top", "20px" )
        , ( "z-index", "102" )
        , ( "align-items", "center" )
        , ( "justify-content", "center" ) ]

navbarTitleStyle: Attribute
navbarTitleStyle =
  style [ ( "letter-spacing", "-3px" )
        , ( "color", "white" )
        , ( "font-size", "25px" )
        , ( "margin-top", "3px") ]

crossStyle: Attribute
crossStyle =
  style [ ( "position", "absolute" )
        , ( "top", "25px" )
        , ( "right", "30px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]
