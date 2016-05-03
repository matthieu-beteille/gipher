module Login.Rest (..) where

import Json.Decode exposing (..)
import ElmFire.Auth exposing (Authentication)
import Login.Types exposing (User)

decodeDisplayName : Decoder String
decodeDisplayName =
  "displayName" := string


getUserFromAuth : Authentication -> User
getUserFromAuth auth =
  User auth.uid auth.token (Result.withDefault "" (decodeValue decodeDisplayName auth.specifics)) Nothing
