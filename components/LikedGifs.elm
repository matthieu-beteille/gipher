module LikedGifs where

import Gif
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import Json.Decode

type alias Model
  = List ( Gif.Model )

type Action
  = Data Gif.Model

firebaseMailbox: Signal.Mailbox Json.Encode.Value
firebaseMailbox =
  Signal.mailbox Json.Encode.null

firebaseSignal: Signal Gif.Model
firebaseSignal =
  Signal.filterMap
    ( \response -> Json.Decode.decodeValue Gif.decodeGifFromFirebase response
                    |> Result.toMaybe )
    (Gif.Model "" "" "" "")
    firebaseMailbox.signal

init: Model
init = []

update: Action -> Model -> Model
update action model =
  case action of
    -- we receive each liked gif one by one from firebase
    Data gif ->
      gif :: model

view: Model -> Html
view model =
  div [ containerStyle ] (List.map Gif.smallView model)

containerStyle: Attribute
containerStyle =
  style [ ( "display", "flex" )
        , ( "flex-wrap", "wrap" )
        , ( "position", "relative")
        , ( "justify-content", "center" )
        , ( "padding-bottom", "30px" )
        , ( "padding-top", "90px" ) ]
