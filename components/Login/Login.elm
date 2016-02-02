module Login where

import ElmFire exposing (childAdded, noOrder)
import ElmFire.Auth exposing (..)
import Task
import Effects exposing (..)
import Json.Encode
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onMouseUp )

type alias User =
  ( { uid: String
    , token: String
    , displayName: String
    , subscription: Maybe (ElmFire.Subscription) } )

type alias Model =
    Maybe ( User )

type Action
  = Login (Maybe Authentication)
  | Logout
  | Subscribed (Maybe ElmFire.Subscription)
  | NoOp

init:Model
init =
  Nothing

update: Signal.Address Json.Encode.Value -> Action -> Model -> ElmFire.Location -> ( Model, Effects Action )
update address action model root =
  case action of
    Login auth ->
      case auth of
        Just auth ->
          let user = (getUserFromAuth auth)
              userObject = Just user
              effects = ElmFire.subscribe
                          (Signal.send address << .value)
                          (always (Task.succeed ()))
                          (childAdded noOrder)
                          (ElmFire.sub user.uid root)
                            |> Task.toMaybe
                            |> Task.map Subscribed
                            |> Effects.task
          in
            ( userObject, effects )

        Nothing ->
          ( model, login root )

    Logout ->
      let effects = case model of
        Just user ->
          case user.subscription of
            Just sub -> ElmFire.unsubscribe sub
                          |> Task.toMaybe
                          |> Task.map (always NoOp)
                          |> Effects.task
            Nothing -> Effects.none

        Nothing -> Effects.none
      in
        ( Nothing, effects )

    NoOp ->
      ( Nothing, Effects.none )

    Subscribed sub ->
      case model of
        Just user ->
          ( Just { user | subscription = sub }, Effects.none )
        Nothing ->
          ( model, Effects.none)

decodeDisplayName: Decoder String
decodeDisplayName =
    "displayName" := string

getUserFromAuth: Authentication -> User
getUserFromAuth auth =
  User auth.uid auth.token (Result.withDefault "" (decodeValue decodeDisplayName auth.specifics)) Nothing

loginView: Signal.Address Action -> Model -> Html
loginView address model =
  div [ containerStyle ] [ h1 [ titleStyle ] [ text "Gipher" ]
          , div [ btnStyle ] [ a [onClick address (Login Nothing)] [ i [ class "material-icons", iconStyle ] [ text "account_circle" ]
                                                                    , text "Login with Facebook"] ] ]

login: ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthPopup "facebook")
    |> Task.toMaybe
    |> Task.map Login
    |> Effects.task

containerStyle: Attribute
containerStyle =
  style [ ( "padding-top", "120px" )
        , ( "text-align", "center" ) ]

iconStyle: Attribute
iconStyle =
  style [ ( "vertical-align", "bottom" )
        , ( "margin-right", "10px" ) ]

titleStyle: Attribute
titleStyle =
  style [ ( "color", "white" )
        , ( "text-align", "center" )
        , ( "margin-top", "65px" )
        , ( "margin-bottom", "50px" )
        , ( "font-size", "2.5em" )
        , ( "letter-spacing", "-3px" ) ]

btnStyle : Attribute
btnStyle =
  style [ ( "font-size", "20px" )
        , ( "color", "white" )
        , ( "cursor", "pointer" )
        , ( "display", "inline-block" )
        , ( "width", "200px" )
        , ( "text-align", "center" )
        , ( "border", "1px solid white" )
        , ( "border-radius", "3px" )
        , ( "padding", "10px" )
        , ( "letter-spacing", "-1px") ]
