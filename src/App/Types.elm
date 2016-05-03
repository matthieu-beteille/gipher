module App.Types (..) where

import ElmFire
import Login
import Stack
import LikedGifs


-- Actions


type Action
  = Stack Stack.Action
  | Login Login.Action
  | LikedGifs LikedGifs.Action
  | Resize ( Int, Int )
  | ToggleMenu
  | GoTo Route
  | NoOp


-- App Routes


type Route
  = Home
  | MyGifs



-- Model


type alias Model =
  { global :
      { root : ElmFire.Location
      , login : Login.Model
      , window : ( Int, Int )
      , isMenuOpened : Bool
      , route : Route
      }
  , newGifs : Stack.Model
  , likedGifs : LikedGifs.Model
  }
