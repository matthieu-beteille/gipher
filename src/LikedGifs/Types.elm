module LikedGifs.Types (..) where

import Gif.Types

type alias Model =
  List (Gif.Types.Model)

type Action
  = Data Gif.Types.Model
