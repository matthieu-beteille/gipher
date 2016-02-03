module LikedGifs where

import Gif
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import Json.Decode
import Effects exposing ( Effects )
import Stack
import Task

type alias Model
  = List ( Gif.Model )

type Action
  = Data Gif.Model

firebaseMailbox: Signal.Mailbox Json.Encode.Value
firebaseMailbox =
  Signal.mailbox Json.Encode.null

firebaseSignal: Signal Action
firebaseSignal =
  Signal.filterMap
    ( \response ->  let gif = Json.Decode.decodeValue Gif.decodeGifFromFirebase response
                              |> Result.toMaybe
                    in
                    case gif of
                      Just gif -> Just (Data gif)
                      Nothing -> Nothing )
    (Data (Gif.Model "" "" "" ""))
    firebaseMailbox.signal

init: Model
init = []

update: Action -> Model -> ( Model, Effects Stack.Action)
update action model =
  case action of
    -- we receive each liked gif one by one from firebase
    -- each time we receive one we remove it from the stack (Stack.Remove)
    Data gif ->
      let effects = Effects.task <| Task.succeed (Stack.Remove gif)
      in
        ( gif :: model, effects )

view: Model -> Html
view model =
  div [ containerStyle ] (List.map Gif.smallView model)

-- flexStyle: Attribute
-- flexStyle =
--   style [ ( "width", "80vw" )
--       , ( "height", "92vw" )
--       , ( "flex-wrap", "wrap" )
--       , ( "display", "flex" )
--       , ( "flex-direction", "column" )
--       , ( "align-content", "center" ) ]

containerStyle: Attribute
containerStyle =
  style [ ( "display", "flex" )
        , ( "flex-wrap", "wrap" )
        , ( "direction", "column" )
        , ( "position", "relative")
        , ( "justify-content", "center" )
        , ( "padding-bottom", "30px" )
        , ( "padding-top", "90px" ) ]
