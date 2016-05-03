module LikedGifs.View (..) where

import LikedGifs.Types exposing (..)
import Gif.View
import Html exposing (..)
import Html.Attributes exposing (..)

view : Model -> Html
view model =
  div [ containerStyle ] (List.map Gif.View.smallView model)


-- flexStyle: Attribute
-- flexStyle =
--   style [ ( "width", "80vw" )
--       , ( "height", "92vw" )
--       , ( "flex-wrap", "wrap" )
--       , ( "display", "flex" )
--       , ( "flex-direction", "column" )
--       , ( "align-content", "center" ) ]


containerStyle : Attribute
containerStyle =
  style
    [ ( "display", "flex" )
    , ( "flex-wrap", "wrap" )
    , ( "direction", "column" )
    , ( "position", "relative" )
    , ( "justify-content", "center" )
    , ( "padding-bottom", "30px" )
    , ( "padding-top", "90px" )
    ]
