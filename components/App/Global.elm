module Global where

import ElmFire
import Login

type alias Model =
  { root: ElmFire.Location
  , user: Login.Model 
  , mouse: ( Int, Int )
  , window: ( Int, Int ) }
