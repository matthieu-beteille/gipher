module StackCard.Inputs (..) where

import Signal.Extra exposing (keepWhen)
import Mouse

draggingMailbox : Signal.Mailbox Bool
draggingMailbox =
  Signal.mailbox False


draggingSignal : Signal ( Int, Int )
draggingSignal =
  keepWhen draggingMailbox.signal
          ( 0, 0 )
          ( Signal.merge touchMailBox.signal Mouse.position )


touchMailBox : Signal.Mailbox ( Int, Int )
touchMailBox =
  Signal.mailbox ( 0, 0 )


nextGifMailbox : Signal.Mailbox Bool
nextGifMailbox =
  Signal.mailbox False
