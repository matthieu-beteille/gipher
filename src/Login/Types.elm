module Login.Types (..) where

import ElmFire
import Signup.Types
import ElmFire.Auth exposing (Authentication)


type Action
  = LoginRequest
  | Login Authentication
  | Logout
  | Subscribed (Maybe ElmFire.Subscription)
  | Signup Signup.Types.Action
  | NoOp


type alias User =
  { uid : String
  , token : String
  , displayName : String
  , subscription : Maybe (ElmFire.Subscription)
  }


type alias Model =
  { user : Maybe (User)
  , signup : Signup.Types.Model
  }
