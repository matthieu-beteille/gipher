module LikedGifs where

import Gif
import Debug

type alias Model
  = List ( Gif.Model )

type Action
  = Data Gif.Model

init = []

update: Action -> Model -> Model
update action model =
  case action of
    Data gif ->
        gif :: model

-- view: Signal.Address Action -> Model -> Html
-- view address model =
--
