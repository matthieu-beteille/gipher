module Login.Inputs (..) where

import ElmFire.Auth
import Login.Types exposing (..)

loginBox : Signal.Mailbox (Maybe ElmFire.Auth.Authentication)
loginBox =
  Signal.mailbox Nothing


loginSignal : Signal Action
loginSignal =
  Signal.filterMap
    (\auth ->
      case auth of
        Just auth ->
          Just (Login auth)

        Nothing ->
          Nothing
    )
    NoOp
    loginBox.signal
