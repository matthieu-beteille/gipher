module Signup.Types (..) where

import ElmFire.Auth exposing (Authentication)

type alias Model =
  { username : String
  , password : String
  , error : String
  , success : String
  }

type Action
  = TypeUsername String
  | TypePassword String
  | Signup
  | Success (Maybe String)
  | Error
  | Login
  | LoginResult (Maybe Authentication)
  | NoOp
