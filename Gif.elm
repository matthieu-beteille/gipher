module Gif where

import Json.Decode as Json
import Http exposing (..)
import Html exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing (onClick)


type alias Model = Maybe (String)

init: (Model, Effects Action)
init = (Nothing, fetchNewGif)

type Action = Fetch | NewGif (Maybe String)

fetchNewGif: Effects Action
fetchNewGif =
  Http.get decodeImageUrl getUrl
    |> Task.toMaybe
    |> Task.map NewGif
    |> Effects.task

getUrl:  String
getUrl =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ ("api_key", "dc6zaTOxFJmzC") ]

decodeImageUrl : Json.Decoder String
decodeImageUrl =
  Json.at ["data", "image_url"] Json.string

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fetch -> (model, fetchNewGif)

    NewGif maybeUrl -> (maybeUrl, Effects.none)

view: Signal.Address Action -> Model -> Html
view address model =
  div [] [div [] [text (Maybe.withDefault "" model)]
  , button [onClick address Fetch] [text "NewGif"]]
