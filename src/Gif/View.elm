module Gif.View (..) where

import Gif.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Regex


cardView : Model -> Html
cardView gif =
  div [ getContainerStyle ] [ div [ getImgStyle gif ] [] ]


stackView : Int -> Model -> Html
stackView index model =
  div [ stackCardStyle model index ] [ div [ getContainerStyle ] [ div [ getImgStyle model ] [] ] ]


smallView : Model -> Html
smallView gif =
  let
    height =
      gif.height
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200

    width =
      gif.width
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200

    newWidth =
      (toFloat width) / 1.2

    newHeight =
      (toFloat height) / 1.2
  in
    a
      [ href gif.url
      , target "_blank"
      , imgLinkStyle
      ]
      [ img
          [ src gif.url
          , style
              [ ( "display", "block" )
              , ( "margin", "1px" )
              , ( "border-radius", "2px" )
              , ( "height", (toString newHeight) ++ "px" )
              , ( "width", (toString newWidth) ++ "px" )
              ]
          ]
          []
      ]



-- modalView: Model -> Html
-- cardView gif =
--   div [ getModalStyle ] [ div [ getImgStyle gif ] [] ]
-- Style


getFixedWidthUrl : String -> String
getFixedWidthUrl =
  Regex.replace Regex.All (Regex.regex "[.]gif") (\_ -> "w.gif")


imgLinkStyle : Attribute
imgLinkStyle =
  style
    [ ( "display", "block" )
    , ( "height", "100%" )
    ]


getContainerStyle : Attribute
getContainerStyle =
  style
    [ ( "box-shadow", "0px 0px 4px 0px #0076E5" )
    , ( "overflow-x", "hidden" )
    , ( "padding", "5px" )
    , ( "background-color", "white" )
    , ( "cursor", "pointer" )
    , ( "border-radius", "3px" )
    ]


getModalStyle : Attribute
getModalStyle =
  style
    [ ( "position", "fixed" )
    , ( "padding", "10px" )
    , ( "background-color", "white" )
    , ( "border-radius", "3px" )
    ]


stackCardStyle : Model -> Int -> Attribute
stackCardStyle gif index =
  let
    height =
      gif.height
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 200

    scaleRatio =
      1 - (((toFloat index) + 1) * 1.0e-2)

    offsetX =
      toString 40

    offsetY =
      toString (13 + height - (3 * (1 + index)) + 40)
  in
    style
      [ ( "position", "absolute" )
      , ( "transform", "translate3d(" ++ offsetX ++ "px, -" ++ offsetY ++ "px, 0px) scale(" ++ (toString scaleRatio) ++ ")" )
      ]


getImgStyle : Model -> Attribute
getImgStyle model =
  let
    { width, height, url } =
      model
  in
    style
      [ ( "width", "200px" )
      , ( "height", height ++ "px" )
      , ( "backgroundImage", "url(" ++ url ++ ")" )
      , ( "backgroundPosition", "center center" )
      , ( "border-radius", "3px" )
      ]
