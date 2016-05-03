module StackCard.Rest (..) where

import StackCard.Types exposing (..)
import Json.Decode as Json exposing ((:=))
import Time exposing (Time, millisecond)
import Gif.Rest

decodeModel : Json.Decoder Model
decodeModel =
  Json.object2
    Model
    decodeAnimationModel
    Gif.Rest.decodeGifFromGiphy


decodeAnimationModel : Json.Decoder AnimationModel
decodeAnimationModel =
  Json.object7
    AnimationModel
    (Json.succeed None)
    (Json.succeed (0 * millisecond))
    (Json.succeed (0 * millisecond))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))
    (Json.succeed ( 0, 0 ))
