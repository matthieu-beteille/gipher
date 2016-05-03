module App.View (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseUp)
import App.Types exposing (..)
import LikedGifs
import Stack
import Login

-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    overflowY =
      model.global.route == MyGifs

    view =
      case model.global.route of
        Home ->
          Stack.view (Signal.forwardTo address Stack) model.newGifs model.global

        MyGifs ->
          LikedGifs.view model.likedGifs

    body =
      case model.global.login.user of
        Just user ->
          [ navBar address model.global.isMenuOpened
          , overlayMenu address model.global.isMenuOpened
          , view
          ]

        Nothing ->
          [ Login.loginView (Signal.forwardTo address Login) model.global.login
          , privacyLink
          ]
  in
    div
      [ containerStyle overflowY ]
      (githubLink :: body)


githubLink : Html
githubLink =
  a
    [ href "https://github.com/matthieu-beteille/gipher"
    , target "_blank"
    , style
        [ ( "position", "absolute" )
        , ( "right", "20px" )
        , ( "bottom", "10px" )
        , ( "z-index", "100" )
        ]
    ]
    [ img
        [ src "https://cdn0.iconfinder.com/data/icons/octicons/1024/mark-github-128.png"
        , style [ ( "width", "30px" ) ]
        ]
        []
    ]


privacyLink : Html
privacyLink =
  div
    [ style
        [ ( "position", "absolute" )
        , ( "bottom", "20px" )
        , ( "font-size", "10px" )
        , ( "color", "white" )
        , ( "text-align", "center" )
        , ( "width", "100%" )
        , ( "z-index", "1" )
        ]
    ]
    [ a
        [ style
            [ ( "color", "white" ) ]
        , href "https://www.iubenda.com/privacy-policy/7822896"
        , target "_blank"
        ]
        [ text "Privacy Policy" ]
    ]


navBar : Signal.Address Action -> Bool -> Html
navBar address isMenuOpened =
  let
    closeMenuIcon =
      if isMenuOpened then
        i [ class "material-icons hover", crossStyle, (onClick address ToggleMenu) ] [ text "clear" ]
      else
        div [] []
  in
    div
      [ navbarStyle ]
      [ div [ onClick address ToggleMenu, class "material-icons hover", hamburgerStyle ] [ text "menu" ]
      , div [ navbarTitleStyle ] [ text "Gipher" ]
      , closeMenuIcon
      ]


overlayMenu : Signal.Address Action -> Bool -> Html
overlayMenu address isOpened =
  div
    [ overlayStyle isOpened ]
    [ div [ class "hover", menuItemStyle, onClick address (GoTo Home) ] [ text "Home" ]
    , div [ class "hover", menuItemStyle, onClick address (GoTo MyGifs) ] [ text "Liked Gifs" ]
    , div [ class "hover", menuItemStyle, onClick address (Login Login.Logout) ] [ text "Logout" ]
    ]


head : Html
head =
  node
    "head"
    []
    [ css "gipher.css"
    , meta
    , font
    , icons
    ]


css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []


font : Html
font =
  node
    "link"
    [ href "https://fonts.googleapis.com/css?family=Source+Sans+Pro"
    , rel "stylesheet"
    ]
    []


icons : Html
icons =
  node
    "link"
    [ href "https://fonts.googleapis.com/icon?family=Material+Icons"
    , rel "stylesheet"
    ]
    []


meta : Html
meta =
  node
    "meta"
    [ name "viewport"
    , content "width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimal-ui"
    ]
    []


containerStyle : Bool -> Attribute
containerStyle overflowY =
  let
    overflow =
      if overflowY then
        [ ( "overflow-y", "auto" )
        , ( "-webkit-overflow-scrolling", "touch" )
        ]
      else
        [ ( "overflow-y", "hidden" ) ]
  in
    style
      (List.concat
        [ overflow
        , [ ( "overflow-x", "hidden" )
          , ( "display", "flex" )
          , ( "flex-direction", "column" )
          , ( "justify-content", "center" )
          , ( "align-items", "center" )
          , ( "font-family", "Source Sans Pro" )
          , ( "background-color", "#0076E5" )
          , ( "height", "100%" )
          ]
        ]
      )


overlayStyle : Bool -> Attribute
overlayStyle isOpened =
  let
    translateValue =
      if isOpened then
        "0%"
      else
        "150%"

    opacity =
      if isOpened then
        "0.97"
      else
        "0"

    transition =
      "transform 10ms ease 100ms, opacity 250ms cubic-bezier(.165,.84,.44,1)"
  in
    style
      [ ( "position", "fixed" )
      , ( "transition", transition )
      , ( "transform", "translate3d(0," ++ translateValue ++ ", 0)" )
      , ( "background-color", "#00FF95" )
      , ( "z-index", "101" )
      , ( "top", "0" )
      , ( "left", "0" )
      , ( "width", "100%" )
      , ( "height", "100%" )
      , ( "opacity", opacity )
      ]


menuItemStyle : Attribute
menuItemStyle =
  style
    [ ( "position", "relative" )
    , ( "top", "100px" )
    , ( "left", "50px" )
    , ( "line-height", "5pc" )
    , ( "font-size", "40px" )
    , ( "color", "white" )
    , ( "cursor", "pointer" )
    ]


hamburgerStyle : Attribute
hamburgerStyle =
  style
    [ ( "font-size", "40px" )
    , ( "color", "white" )
    , ( "cursor", "pointer" )
    , ( "position", "absolute" )
    , ( "left", "15px" )
    , ( "top", "0px" )
    ]


navbarStyle : Attribute
navbarStyle =
  style
    [ ( "position", "fixed" )
    , ( "display", "flex" )
    , ( "width", "100%" )
    , ( "align-item", "center" )
    , ( "top", "10px" )
    , ( "left", "0px" )
    , ( "z-index", "102" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


navbarTitleStyle : Attribute
navbarTitleStyle =
  style
    [ ( "letter-spacing", "-3px" )
    , ( "color", "white" )
    , ( "font-size", "25px" )
    , ( "margin-top", "3px" )
    ]


crossStyle : Attribute
crossStyle =
  style
    [ ( "position", "absolute" )
    , ( "top", "10px" )
    , ( "right", "15px" )
    , ( "color", "white" )
    , ( "cursor", "pointer" )
    ]
