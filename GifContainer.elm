module GifContainer where

import Json.Decode as Json exposing ((:=))
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Debug
import Gif

type alias Model = List Gif.Model

init: (Model, Effects Action)
init = ([], fetchNewGifs)

type Action = Fetch
  | NewGifs (Maybe Model)
  | Gif Gif.Action

fetchNewGifs: Effects Action
fetchNewGifs =
  Http.get decodeList getUrl
    |> Task.toMaybe
    |> Task.map NewGifs
    |> Effects.task

getUrl: String
getUrl =
  Http.url "http://api.giphy.com/v1/gifs/trending"
    [ ("api_key", "dc6zaTOxFJmzC") ]

decodeList: Json.Decoder Model
decodeList =
  Json.object1 identity
    ("data" := Json.list Gif.decodeModel)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fetch -> init

    NewGifs maybeGifs ->
      case maybeGifs of
        Just gifs -> (gifs, Effects.none)

        Nothing -> (fst init, Effects.none)

    Gif gifAction ->
      case (List.head model) of
        Just gif ->
          case (List.tail model) of
            Just tail ->
            let (gif, effects) = Gif.update gifAction gif
            in
              (gif :: tail, Effects.map Gif effects)

            Nothing -> (model, Effects.none)

        Nothing -> (model, Effects.none)


view: Signal.Address Action -> Model -> Html
view address model =
  div [flexContainerStyle] (List.map (Gif.view (Signal.forwardTo address Gif)) (List.take 1 model))

flexContainerStyle: Attribute
flexContainerStyle =
  style [ ("display", "flex")
        , ("flex-direction", "column")
        , ("align-content", "center")
        , ("justify-content", "center")
        , ("align-items", "center") ]

btnAttributes: Signal.Address Action -> List (Attribute)
btnAttributes address =
  [ onClick address Fetch
  , style
    [ ("font-size", "20px")
    , ("color", "white")
    , ("cursor", "pointer")
    , ("display", "inline-block")
    , ("width", "100px")
    , ("text-align", "center")
    , ("border", "1px solid white")
    , ("border-radius", "3px")
    , ("padding", "10px")
    , ("margin-top", "20px")
    ]]
