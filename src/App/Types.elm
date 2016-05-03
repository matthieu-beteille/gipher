module App.Types (..) where

import ElmFire
import Login.Types
import Stack.Types
import LikedGifs.Types


type Action
  = Stack Stack.Types.Action
  | Login Login.Types.Action
  | LikedGifs LikedGifs.Types.Action
  | Resize ( Int, Int )
  | ToggleMenu
  | GoTo Route
  | NoOp


type Route
  = Home
  | MyGifs


type alias Global =
  { root : ElmFire.Location
  , login : Login.Types.Model
  , window : ( Int, Int )
  , isMenuOpened : Bool
  , route : Route
  }


type alias Model =
  { global : Global
  , newGifs : Stack.Types.Model
  , likedGifs : LikedGifs.Types.Model
  }
