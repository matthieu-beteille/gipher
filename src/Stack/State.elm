module Stack.State (..) where

import Stack.Types exposing (..)
import Stack.Rest exposing (..)
import App.Types
import Effects exposing (..)
import Task exposing (..)
import StackCard.State
import ElmFire
import Gif.Types
import Gif.Rest

init : ( Model, Effects Action )
init =
  ( [], fetchNewGifs )

removeLikedGifs : List (Gif.Types.Model) -> Model -> Model
removeLikedGifs likedGifs gifs =
  let
    idsList =
      List.map (\gif -> gif.id) likedGifs

    filteredList =
      List.filter (\item -> not (List.member item.gif.id idsList)) gifs
  in
    filteredList


update : Action -> Model -> List (Gif.Types.Model) -> App.Types.Global -> ( Model, Effects Action )
update action model likedGifs global =
  case action of
    NewGifs maybeGifs ->
      case maybeGifs of
        Just gifs ->
          let
            filteredGifs =
              removeLikedGifs likedGifs gifs
          in
            ( filteredGifs, Effects.none )

        Nothing ->
          init

    StackCard gifAction ->
      case (List.head model) of
        Just gif ->
          let
            ( gif, effects ) =
              StackCard.State.update gifAction gif global
          in
            ( gif :: List.drop 1 model, Effects.map StackCard effects )

        Nothing ->
          ( model, Effects.none )

    Remove gif ->
      let
        newModel =
          List.filter (\item -> not (item.gif.id == gif.id)) model
      in
        ( newModel, Effects.none )

    NextCard hasBeenLiked ->
      let
        effects =
          case (List.head model) of
            Just gif ->
              case global.login.user of
                Just user ->
                  let
                    firebaseLocation =
                      ElmFire.sub ("likedGifs/" ++ user.uid) global.root
                  in
                    if hasBeenLiked then
                      ElmFire.set (Gif.Rest.encodeGif gif.gif) (ElmFire.push firebaseLocation)
                        |> Task.toMaybe
                        |> Task.map (\_ -> NoOp)
                        |> Effects.task
                    else
                      Effects.none

                Nothing ->
                  Effects.none

            Nothing ->
              Effects.none
      in
        ( List.drop 1 model, effects )

    NoOp ->
      ( model, Effects.none )
