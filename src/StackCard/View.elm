module StackCard.View (..) where

import StackCard.Types exposing (..)
import StackCard.Inputs exposing (touchMailBox)
import StackCard.State exposing (duration)
import Gif.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Json.Decode as Json exposing ((:=))
import Easing exposing (..)
import Time exposing (Time, millisecond)

-- easing


easeBack : Time -> Float -> Float -> Float
easeBack currentTime start end =
  ease easeOutBounce float start end duration currentTime


easeOpacity : Time -> Float -> Float -> Float
easeOpacity currentTime start end =
  ease easeOutExpo float start end duration currentTime



-- delta calculation


getDelta : AnimationModel -> ( Int, Int ) -> ( Float, Float )
getDelta animationModel mousePos =
  let
    { startPos, endPos, animationType, elapsedTime } =
      animationModel

    ( mouseX, mouseY ) =
      mousePos

    ( startX, startY ) =
      startPos

    ( endPosX, endPosY ) =
      endPos
  in
    case animationType of
      MouseDragging ->
        ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )

      DragBack ->
        let
          ( dx, dy ) =
            ( (toFloat mouseX) - startX, (toFloat mouseY) - startY )
        in
          ( (easeBack elapsedTime endPosX 0), (easeBack elapsedTime endPosY 0) )

      Swipe ->
        ( (easeBack elapsedTime 0 (fst endPos)), startY )

      FadeOut ->
        endPos

      None ->
        ( 0, 0 )



-- view


view : Signal.Address Action -> Model -> Html
view address model =
  let
    { startPos, endPos, animationType, elapsedTime, mouse } =
      model.animationState

    delta =
      getDelta model.animationState mouse

    cardAttributes =
      getCardAttributes
        model
        delta
        address

    dx =
      fst delta

    tag =
      if (abs dx) < 10 then
        div [] []
      else
        tagElement (dx > 0)
  in
    div (getContainerAttributes delta address) [ div cardAttributes [ Gif.View.cardView model.gif, tag ] ]


tagElement : Bool -> Html
tagElement liked =
  let
    label =
      if liked then
        "LIKED"
      else
        "NOPE"

    color =
      if liked then
        "#00FF95"
      else
        "#FF2300"
  in
    div
      [ style
          [ ( "position", "absolute" )
          , ( "top", "25px" )
          , ( "transform", "rotateZ(-25deg)" )
          , ( "left", "20px" )
          , ( "font-size", "30px" )
          , ( "color", color )
          , ( "border", "3px solid" )
          , ( "border-radius", "10px" )
          , ( "padding", "5px" )
          , ( "opacity", "0.8" )
          ]
      ]
      [ text label ]



-- event decoders


decoder : Json.Decoder ( Int, Int )
decoder =
  Json.object2 (,) ("pageX" := Json.int) ("pageY" := Json.int)


doubleTuple : Float -> Float -> Float -> Float -> ( ( Float, Float ), ( Float, Float ) )
doubleTuple a b c d =
  ( ( a, b ), ( c, d ) )


relativeDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ) )
relativeDecoder =
  Json.object4
    doubleTuple
    ("pageX" := Json.float)
    ("pageY" := Json.float)
    ("offsetX" := Json.float)
    ("offsetY" := Json.float)

touchDecoder : Json.Decoder ( Int, Int )
touchDecoder =
  Json.object1 (\test -> test) (Json.at [ "touches", "0" ] decoder)

getContainerAttributes : ( Float, Float ) -> Signal.Address Action -> List (Attribute)
getContainerAttributes delta address =
  [ Html.Events.on "mouseup" decoder (\val -> Signal.message address (DragEnd delta))
  , Html.Events.on "touchend" Json.value (\val -> Signal.message address (DragEnd delta))
  , style [ ( "padding", "40px" ) ]
  ]

getCardAttributes : Model -> ( Float, Float ) -> Signal.Address Action -> List (Attribute)
getCardAttributes model delta address =
  let options =  { stopPropagation = False
                  , preventDefault = True }
      listeners = if model.animationState.animationType /= None then
        [ ]
      else
       [ Html.Events.on "mousedown" relativeDecoder (\val -> Signal.message address (DragStart val))
       , Html.Events.on "touchstart" touchDecoder (\( x, y ) -> Signal.message address (DragStart ( ( toFloat x, toFloat y ), ( 0, 0 ) )))
       ]
  in
    List.concat [ listeners, [ Html.Events.onWithOptions "touchmove" options touchDecoder (\val -> Signal.message touchMailBox.address val)
                              , style (getCardStyle model delta) ] ]


getCardStyle : Model -> ( Float, Float ) -> List ( String, String )
getCardStyle model ( dx, dy ) =
  let
    { elapsedTime, animationType, relativeStartPos } =
      model.animationState

    ( relX, relY ) =
      relativeStartPos

    gifOpacity =
      if animationType == FadeOut then
        toString (easeOpacity elapsedTime 1 0)
      else
        "1"

    transform =
      translateAndRotate dx dy relX relY
  in
    ( "opacity", gifOpacity )
      :: ( "z-index", "100" )
      :: ( "position", "relative" )
      :: List.concat [ transform ]


translateAndRotate : Float -> Float -> Float -> Float -> List ( String, String )
translateAndRotate dx dy relX relY =
  let
    limit =
      100

    coefX =
      if dx > limit then
        limit
      else if dx < -limit then
        -limit
      else
        dx

    coefY =
      if dy > limit then
        limit
      else if dy < -limit then
        -limit
      else
        dy
  in
    [ ( "transform", "translate3d(" ++ (toString dx) ++ "px, " ++ (toString dy) ++ "px, 0px) rotate3d(0,0,1," ++ (toString (2.0e-3 * coefX * coefY)) ++ "deg)" )
    , ( "transform-origin", (toString relX) ++ "px " ++ (toString relY) ++ "px" )
    ]
