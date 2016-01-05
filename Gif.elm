module Gif where

import Json.Decode as Json
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick)

type alias Model = { url: String
                    , width: String
                    , height: String }

init: (Model, Effects Action)
init = (Model "loading" "100" "100", fetchNewGif)

type Action = Fetch
  | NewGif (Maybe Model)

fetchNewGif: Effects Action
fetchNewGif =
  Http.get decodeImageUrl getUrl
    |> Task.toMaybe
    |> Task.map NewGif
    |> Effects.task

getUrl: String
getUrl =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ ("api_key", "dc6zaTOxFJmzC") ]

decodeImageUrl: Json.Decoder Model
decodeImageUrl =
  Json.object3 Model
    (Json.at ["data", "image_url"] Json.string)
    (Json.at ["data", "image_width"] Json.string)
    (Json.at ["data", "image_height"] Json.string)

-- decodeGif: Json.Decoder

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fetch -> init

    NewGif maybeGif ->
      case maybeGif of
        Just gif ->
          (gif, Effects.none)

        Nothing -> (fst init, Effects.none)

view: Signal.Address Action -> Model -> Html
view address model =
  let url =  model.url
  in
    div [containerStyle] [img (getGifAttributes model) []
    , button (btnAttributes address) [text "NewGif"]]

containerStyle: Attribute
containerStyle =
  style [ ("display", "flex")
        , ("flex-direction", "column")
        , ("align-content", "center")
        , ("justify-content", "center")
        , ("align-items", "center") ]

getGifAttributes: Model -> List (Attribute)
getGifAttributes model =
  [ src model.url
  , style [ ("width", model.width)
          , ("height", model.height) ]]

btnAttributes : Signal.Address Action -> List (Attribute)
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
