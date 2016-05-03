module Stack.Rest (..) where

import Stack.Types exposing (..)
import StackCard.Rest exposing (..)
import Effects exposing (..)
import Task
import Json.Decode as Json exposing ((:=))
import Http

fetchNewGifs : Effects Action
fetchNewGifs =
  Http.get decodeList getUrl
    |> Task.toMaybe
    |> Task.map NewGifs
    |> Effects.task


getUrl : String
getUrl =
  Http.url
    "https://api.giphy.com/v1/gifs/trending"
    [ ( "api_key", "dc6zaTOxFJmzC" ), ( "limit", "400" ) ]


decodeList : Json.Decoder Model
decodeList =
  Json.object1
    identity
    ("data" := Json.list StackCard.Rest.decodeModel)
