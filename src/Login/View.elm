module Login.View (..) where

import Login.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseUp)
import Signup.View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    icon =
      i [ class "material-icons", iconStyle ] [ text "account_circle" ]
  in
    div
      [ containerStyle ]
      [ h1 [ titleStyle ] [ text "Gipher" ]
      , Signup.View.view (Signal.forwardTo address Signup) model.signup
      , div
          [ btnStyle, class "login-btn", onClick address LoginRequest ]
          [ icon, text "Login with Facebook" ]
      ]


containerStyle : Attribute
containerStyle =
  style [ ( "text-align", "center" ) ]


iconStyle : Attribute
iconStyle =
  style
    [ ( "vertical-align", "bottom" )
    , ( "margin-right", "10px" )
    ]


titleStyle : Attribute
titleStyle =
  style
    [ ( "color", "white" )
    , ( "text-align", "center" )
    , ( "margin-top", "0px" )
    , ( "margin-bottom", "50px" )
    , ( "font-size", "2.5em" )
    , ( "letter-spacing", "-3px" )
    ]


btnStyle : Attribute
btnStyle =
  style
    [ ( "font-size", "18px" )
    , ( "cursor", "pointer" )
    , ( "display", "inline-block" )
    , ( "width", "220px" )
    , ( "text-align", "center" )
    , ( "border", "1px solid white" )
    , ( "border-radius", "3px" )
    , ( "padding", "10px" )
    , ( "margin-top", "20px" )
    , ( "letter-spacing", "-1px" )
    ]
