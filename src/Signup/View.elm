module Signup.View (..) where

import Signup.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)

renderForm : Signal.Address Action -> Html
renderForm address =
  Html.form
    [ class "col s12" ]
    [ div
        [ class "row" ]
        [ div
            [ class "input-field col s12" ]
            [ input
                [ class "validate"
                , type' "text"
                , placeholder "Email Address"
                , on "input" targetValue (\value -> Signal.message address (TypeUsername value))
                ]
                []
            ]
        ]
    , div
        [ class "row" ]
        [ div
            [ class "input-field col s12" ]
            [ input
                [ class "validate"
                , type' "password"
                , placeholder "Password"
                , on "input" targetValue (\value -> Signal.message address (TypePassword value))
                ]
                []
            ]
        ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    message =
      if model.success /= "" then
        div [ successStyle ] [ text model.success ]
      else
        div [ errorStyle ] [ text model.error ]
  in
    div
      []
      [ message
      , renderForm address
      , div
          [ btnStyle, class "login-btn", onClick address Login ]
          [ text "Sign-in" ]
      , div
          [ btnStyle, class "login-btn", onClick address Signup ]
          [ text "Sign-up" ]
      ]


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


errorStyle : Attribute
errorStyle =
  style
    [ ( "color", "#FF2300" )
    , ( "text-align", "center" )
    , ( "margin-top", "10px" )
    , ( "margin-bottom", "10px" )
    , ( "font-size", "1em" )
    ]

successStyle : Attribute
successStyle =
  style
    [ ( "color", "#00FF95" )
    , ( "text-align", "center" )
    , ( "margin-top", "10px" )
    , ( "margin-bottom", "10px" )
    , ( "font-size", "1em" )
    ]


btnStyle : Attribute
btnStyle =
  style
    [ ( "font-size", "18px" )
    , ( "cursor", "pointer" )
    , ( "display", "inline-block" )
    , ( "width", "100px" )
    , ( "text-align", "center" )
    , ( "border", "1px solid white" )
    , ( "border-radius", "3px" )
    , ( "padding", "10px" )
    , ( "margin-top", "20px" )
    , ( "margin-right", "5px" )
    , ( "margin-left", "5px" )
    , ( "letter-spacing", "-1px" )
    ]
