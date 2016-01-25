module LikedGifs where

import Gif
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model
  = List ( Gif.Model )

type Action
  = Data Gif.Model

init = []

update: Action -> Model -> ( Model, String )
update action model =
  case action of
    -- we receive each liked gif one by one from firebase
    Data gif ->
        ( gif :: model, gif.id )

view: Signal.Address Action -> Model -> Html
view address model =
  div [ containerStyle ] (List.map Gif.cardView model)

containerStyle =
  style [ ( "display", "flex" )
        , ( "flex-wrap", "wrap" )
        , ( "position", "relative")
        , ( "justify-content", "center" )
        , ( "padding-bottom", "30px" )
        , ( "padding-top", "90px" ) ]
