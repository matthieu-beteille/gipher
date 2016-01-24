module Global where

import ElmFire

type alias User =
  { uid: String
  , token: String
  , displayName: String
  }

type alias Model = { root: ElmFire.Location
          , user: Maybe ( User )
          , mouse: ( Int, Int )
          , window: ( Int, Int ) }
