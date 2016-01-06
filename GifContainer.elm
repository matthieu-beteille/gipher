module GifContainer where

import Json.Decode as Json exposing ((:=))
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Debug

type alias Gif = { url: String
                  , width: String
                  , height: String }

type alias Model = List Gif

init: (Model, Effects Action)
init = ([], fetchNewGifs)

type Action = Fetch
  | NewGifs (Maybe Model)

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

decodeGif: Json.Decoder Gif
decodeGif =
  Json.object3 Gif
    (Json.at ["images", "fixed_width", "url"] Json.string)
    (Json.at ["images", "fixed_width", "width"] Json.string)
    (Json.at ["images", "fixed_width", "height"] Json.string)

decodeList: Json.Decoder Model
decodeList =
  Json.object1 identity
    ("data" := Json.list decodeGif)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fetch -> init

    NewGifs maybeGifs ->
      case maybeGifs of
        Just gifs ->
          (gifs, Effects.none)

        Nothing -> (fst init, Effects.none)

view: Signal.Address Action -> Model -> Html
view address model =
  div [flexContainerStyle] (List.map gifView (List.take 1 model))

gifView gif =
  img (getGifAttributes gif) []

flexContainerStyle: Attribute
flexContainerStyle =
  style [ ("display", "flex")
        , ("flex-direction", "column")
        , ("align-content", "center")
        , ("justify-content", "center")
        , ("align-items", "center") ]

getGifAttributes: Gif -> List (Attribute)
getGifAttributes model =
  [ src model.url
  -- , onMouseDown Debug.log "loool"
  , style [ ("width", model.width)
          , ("height", model.height)
          ]]

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
