module Stack where

import Json.Decode as Json exposing ( (:=) )
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html.Events exposing ( onClick, onMouseDown, onMouseUp )
import Debug
import StackCard
import Result
import String
import ElmFire
import Json.Encode
import Gif
import LikedGifs

type alias Model = List StackCard.Model

init: Bool -> ( Model, Effects Action )
init requestGifs =
  if requestGifs then
    ( [], fetchNewGifs )
  else
    ( [], Effects.none )

type Action = Fetch
  | NewGifs (Maybe Model)
  | StackCard StackCard.Action

fetchNewGifs: Effects Action
fetchNewGifs =
  Http.get decodeList getUrl
    |> Task.toMaybe
    |> Task.map NewGifs
    |> Effects.task

getUrl: String
getUrl =
  Http.url
    "http://api.giphy.com/v1/gifs/trending"
    [ ( "api_key", "dc6zaTOxFJmzC" ), ( "limit", "400" ) ]

decodeList: Json.Decoder Model
decodeList =
  Json.object1 identity
    ( "data" := Json.list StackCard.decodeModel )

removeLikedGifs: LikedGifs.Model -> Model -> Model
removeLikedGifs likedGifs gifs =
  let idsList = List.map (\gif -> gif.id) likedGifs
      filteredList = List.filter (\item -> not (List.member item.gif.id idsList)) gifs
  in
    filteredList

removeById: String -> Model -> Model
removeById id gifs =
  List.filter (\item -> not (item.gif.id == id)) gifs

update: Action
        -> Model
        -> LikedGifs.Model
        -> { c | root : ElmFire.Location, user : Maybe { a | uid : String }, window : ( Int, Int ) }
        -> ( Model, Effects Action )
update action model likedGifs global =
  case action of
    Fetch ->
      init True

    NewGifs maybeGifs ->
      case maybeGifs of
        Just gifs ->
          let filteredList = removeLikedGifs likedGifs gifs
          in
            ( gifs, Effects.none )

        Nothing ->
          ( init False )

    StackCard gifAction ->
      case (List.head model) of
        Just gif ->
          case (List.tail model) of
            Just tail ->
            let ( ( gif, result ), gifEffects ) = StackCard.update gifAction gif global
                next = case result of
                  0 ->
                    False
                  _ ->
                    True
                create = case global.user of
                  Just user ->
                    let firebaseLocation = ElmFire.sub user.uid global.root
                    in
                      if result == 1 then
                        ElmFire.set (Gif.encodeGif gif.gif) (ElmFire.push firebaseLocation)
                          |> Task.toMaybe
                          |> Task.map StackCard.NoOp
                          |> Effects.task
                      else
                        Effects.none

                  Nothing -> Effects.none

                effects = Effects.batch [ create, gifEffects ]
            in
              if not next then
                ( gif :: tail, Effects.map StackCard effects )
              else
                ( tail, Effects.map StackCard effects )

            Nothing -> ( model, Effects.none )

        Nothing -> ( model, Effects.none )


view address model global =
  let currentGif = List.head model
      tail = List.tail model
      error = div [ flexContainerStyle ] [ div [] [ text "No more gif" ] ]
      gifComponent = case currentGif of
        Just first ->
          case tail of
            Just tail ->
              div []
                (div [] [ StackCard.view (Signal.forwardTo address StackCard) True global 0 first ] ::
                (List.reverse (List.indexedMap (StackCard.view (Signal.forwardTo address StackCard) False global)
                                               (List.take 5 tail))))
            Nothing -> error

        Nothing -> error

  in
    div [ flexContainerStyle ] [ gifComponent
            , div [ buttonsContainer ]
              [ i [ class "material-icons hover-btn", crossStyle, onClick address (StackCard StackCard.SwipeLeft) ] [text "clear"]
              , i [ class "material-icons hover-btn", tickStyle, onClick address (StackCard StackCard.SwipeRight) ] [text "favorite"] ] ]

crossStyle: Attribute
crossStyle =
  style [ ("font-size", "50px" )
        , ("color", "#FF2300" )
        , ("cursor", "pointer" )
        , ("padding", "9px 11px" )
        , ("transform", "translateX(2px)" )
        , ("border", "6px solid #BBBFBE" )
        , ("font-weight", "bold" )
        , ("border-radius", "50%" ) ]

tickStyle: Attribute
tickStyle =
  style [ ("font-size", "50px" )
        , ("color", "#00FF95" )
        , ("cursor", "pointer" )
        , ("padding", "9px 11px" )
        , ("transform", "translateX(-2px)" )
        , ("border", "6px solid #BBBFBE" )
        , ("border-radius", "50%" ) ]

buttonsContainer: Attribute
buttonsContainer =
  style [ ( "font-size", "50px" )
        , ( "display", "flex" )
        , ( "margin-top", "60px" )
        , ( "justify-content", "space-around" ) ]

flexContainerStyle: Attribute
flexContainerStyle =
  style [ ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "align-content", "center" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" )
        , ( "padding-top", "150px") ]

btnAttributes: Signal.Address Action -> List ( Attribute )
btnAttributes address =
  [ onClick address Fetch
  , style
    [ ( "font-size", "20px" )
    , ( "color", "white" )
    , ( "cursor", "pointer" )
    , ( "display", "inline-block" )
    , ( "width", "100px" )
    , ( "text-align", "center" )
    , ( "border", "1px solid white" )
    , ( "border-radius", "3px" )
    , ( "padding", "10px" )
    , ( "margin-top", "20px" ) ] ]
