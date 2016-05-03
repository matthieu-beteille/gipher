module Stack.View (..) where

import Stack.Types exposing (..)
import StackCard.Types
import StackCard.View
import Gif.View
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Html exposing (..)
import Html.Attributes exposing (..)

view : Signal.Address Action -> List StackCard.Types.Model -> a -> Html
view address model global =
  let
    currentGif =
      List.head model

    tail =
      List.tail model

    error =
      div [ flexContainerStyle ] [ div [] [ text "No more gifs today" ] ]

    gifComponent =
      case currentGif of
        Just first ->
          case tail of
            Just tail ->
              div
                []
                (StackCard.View.view (Signal.forwardTo address StackCard) first
                  :: (List.reverse
                        (List.indexedMap
                          (\id gif -> Gif.View.stackView id gif.gif)
                          (List.take 5 tail)
                        )
                     )
                )

            Nothing ->
              error

        Nothing ->
          error
  in
    div
      [ flexContainerStyle ]
      [ gifComponent
      , div
          [ buttonsContainer ]
          [ i
              [ class "material-icons hover-btn"
              , crossStyle
              , onClick address (StackCard StackCard.Types.SwipeLeft)
              ]
              [ text "clear" ]
          , i
              [ class "material-icons hover-btn"
              , tickStyle
              , onClick address (StackCard StackCard.Types.SwipeRight)
              ]
              [ text "favorite" ]
          ]
      ]


crossStyle : Attribute
crossStyle =
  style
    [ ( "font-size", "50px" )
    , ( "color", "#FF2300" )
    , ( "margin", "5px" )
    , ( "cursor", "pointer" )
    , ( "padding", "9px 11px" )
    , ( "transform", "translateX(2px)" )
    , ( "border", "6px solid white" )
    , ( "font-weight", "bold" )
    , ( "border-radius", "50%" )
    , ( "-webkit-touch-callout", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-khtml-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    ]


tickStyle : Attribute
tickStyle =
  style
    [ ( "font-size", "50px" )
    , ( "color", "#00FF95" )
    , ( "margin", "5px" )
    , ( "cursor", "pointer" )
    , ( "padding", "9px 11px" )
    , ( "transform", "translateX(-2px)" )
    , ( "border", "6px solid white" )
    , ( "border-radius", "50%" )
    , ( "-webkit-touch-callout", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-khtml-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    ]


buttonsContainer : Attribute
buttonsContainer =
  style
    [ ( "font-size", "50px" )
    , ( "display", "flex" )
    , ( "margin-top", "20px" )
    , ( "justify-content", "space-around" )
    ]


flexContainerStyle : Attribute
flexContainerStyle =
  style
    [ ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "align-content", "center" )
    , ( "justify-content", "center" )
    , ( "align-items", "center" )
    ]
