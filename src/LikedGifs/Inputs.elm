module LikedGifs.Inputs (..) where

import LikedGifs.Types exposing (..)
import Gif.Rest
import Gif.Types
import Json.Encode
import Json.Decode

firebaseMailbox : Signal.Mailbox Json.Encode.Value
firebaseMailbox =
  Signal.mailbox Json.Encode.null


firebaseSignal : Signal Action
firebaseSignal =
  Signal.filterMap
    (\response ->
      let
        gif =
          Json.Decode.decodeValue Gif.Rest.decodeGifFromFirebase response
            |> Result.toMaybe
      in
        case gif of
          Just gif ->
            Just (Data gif)

          Nothing ->
            Nothing
    )
    (Data (Gif.Types.Model "" "" "" ""))
    firebaseMailbox.signal
