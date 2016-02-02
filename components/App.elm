module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onMouseUp )
import ElmFire exposing (..)
import Effects exposing (..)
import ElmFire exposing ( Snapshot, childAdded, noOrder, noLimit )
import Json.Encode
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

init: Bool -> (Model, Effects Action)
init requestGifs =
  let ( gifModel, gifEffect ) = Stack.init requestGifs
  in
    ( { global = { root = ElmFire.fromUrl firebaseUrl
                 , user = Login.init
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
  | Resize ( Int, Int )
  | ToggleMenu
  | GoTo Route
  | NoOp

  -- Update

update: Signal.Address Json.Encode.Value -> Action -> Model -> (Model, Effects Action)
update address action model =
  let { global } = model
  in
    case action of
      Login loginAction ->
        let ( newUser , effects ) = Login.update address loginAction model.global.user model.global.root
            newGlobal = { global | user = newUser }
        in
          if newUser == Nothing then
            let
              ( initState, initEffects ) = init (model.global.user == Nothing)
              initGlobal = initState.global
              newGlobal = { initGlobal | window = model.global.window }
            in
              ( { initState | global = newGlobal }, Effects.batch [ initEffects, (Effects.map Login effects)] )
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
        let ( newLikedGifs, addedId ) = LikedGifs.update action model.likedGifs
            newGifs = Stack.removeById addedId model.newGifs
        in
          ( { model | likedGifs = newLikedGifs, newGifs = newGifs }, Effects.none )

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
        MyGifs -> LikedGifs.view (Signal.forwardTo address LikedGifs) model.likedGifs
      body = case model.global.user of
        Just user ->
          [ navBar address,
            overlayMenu address model.global.isMenuOpened,
            view ]
        Nothing ->
          [ Login.loginView (Signal.forwardTo address Login) model.global.user ]
  in
    div [ containerStyle overflowY ] ( head :: body )

navBar: Signal.Address Action -> Html
navBar address =
  div [ onClick address ToggleMenu, navbarStyle ] [ div [class "material-icons hover", hamburgerStyle] [text "menu"]
                                                  , div [ navbarTitleStyle ] [ text "Gipher" ] ]
overlayMenu: Signal.Address Action -> Bool -> Html
overlayMenu address isOpened =
    div [ overlayStyle isOpened ] [ div [ class "hover", menuItemStyle, onClick address (GoTo Home) ] [text "Home"]
                                   , div [ class "hover", menuItemStyle, onClick address (GoTo MyGifs)] [text "Liked Gifs"]
                                   , div [ class "hover", menuItemStyle, onClick address (Login Login.Logout) ] [ text "Logout" ]
                                   , i [ class "material-icons hover", crossStyle, (onClick address ToggleMenu) ]
                                       [ text "clear" ] ]

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
        , ( "top", "20px" )
        , ( "right", "30px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" ) ]
