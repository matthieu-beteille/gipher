module Login (..) where

import ElmFire exposing (childAdded, noOrder)
import ElmFire.Auth exposing (..)
import Task
import Effects exposing (..)
import Json.Decode exposing (..)
import Effects
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseUp)
import LikedGifs exposing (firebaseMailbox)
import Signup


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


type alias User =
  { uid : String
  , token : String
  , displayName : String
  , subscription : Maybe (ElmFire.Subscription)
  }


type alias Model =
  { user : Maybe (User), signup : Signup.Model }


type Action
  = LoginRequest
  | Login Authentication
  | Logout
  | Subscribed (Maybe ElmFire.Subscription)
  | Signup Signup.Action
  | NoOp


init : ElmFire.Location -> ( Model, Effects Action )
init loc =
  let
    effects =
      subscribeAuth (\auth -> Signal.send loginBox.address auth) loc
        |> Task.toMaybe
        |> Task.map (\_ -> NoOp)
        |> Effects.task
  in
    ( { user = Nothing, signup = Signup.init }, effects )


login : ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthRedirect "facebook")
    |> Task.toMaybe
    |> Task.map (always NoOp)
    |> Effects.task


update : Action -> Model -> ElmFire.Location -> ( Model, Effects Action )
update action model root =
  case action of
    LoginRequest ->
      ( model, login root )

    Login auth ->
      case model.user of
        Nothing ->
          let
            user =
              (getUserFromAuth auth)

            userObject =
              Just user

            effects =
              case user.subscription of
                Nothing ->
                  ElmFire.subscribe
                    (Signal.send firebaseMailbox.address << .value)
                    (always (Task.succeed ()))
                    (childAdded noOrder)
                    (ElmFire.sub ("likedGifs/" ++ user.uid) root)
                    |> Task.toMaybe
                    |> Task.map Subscribed
                    |> Effects.task

                Just sub ->
                  Effects.none
          in
            ( { model | user = userObject }, effects )

        Just user ->
          ( model, Effects.none )

    Logout ->
      let
        effects =
          case model.user of
            Just user ->
              case user.subscription of
                Just sub ->
                  let
                    unsubscribeEffect =
                      ElmFire.unsubscribe sub
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task

                    logoutEffect =
                      ElmFire.Auth.unauthenticate root
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task
                  in
                    Effects.batch [ unsubscribeEffect, logoutEffect ]

                Nothing ->
                  Effects.none

            Nothing ->
              Effects.none
      in
        ( { model | user = Nothing }, effects )

    NoOp ->
      ( model, Effects.none )

    Subscribed sub ->
      case model.user of
        Just user ->
          ( { model | user = Just { user | subscription = sub } }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    Signup signupAction ->
      let
        ( newSignup, effects ) =
          Signup.update signupAction model.signup root
      in
        ( { model | signup = newSignup }, Effects.map Signup effects )


decodeDisplayName : Decoder String
decodeDisplayName =
  "displayName" := string


getUserFromAuth : Authentication -> User
getUserFromAuth auth =
  User auth.uid auth.token (Result.withDefault "" (decodeValue decodeDisplayName auth.specifics)) Nothing


loginView : Signal.Address Action -> Model -> Html
loginView address model =
  let
    icon =
      i [ class "material-icons", iconStyle ] [ text "account_circle" ]
  in
    div
      [ containerStyle ]
      [ h1 [ titleStyle ] [ text "Gipher" ]
      , Signup.view (Signal.forwardTo address Signup) model.signup
      , div
          [ btnStyle, class "login-btn", onClick address LoginRequest ]
          [ icon, text "Login with Facebook" ]
      ]


containerStyle : Attribute
containerStyle =
  style [ ( "text-align", "center" ) ]


iconStyle : Attribute
iconStyle =
  style
    [ ( "vertical-align", "bottom" )
    , ( "margin-right", "10px" )
    ]


titleStyle : Attribute
titleStyle =
  style
    [ ( "color", "white" )
    , ( "text-align", "center" )
    , ( "margin-top", "0px" )
    , ( "margin-bottom", "50px" )
    , ( "font-size", "2.5em" )
    , ( "letter-spacing", "-3px" )
    ]


btnStyle : Attribute
btnStyle =
  style
    [ ( "font-size", "18px" )
    , ( "cursor", "pointer" )
    , ( "display", "inline-block" )
    , ( "width", "220px" )
    , ( "text-align", "center" )
    , ( "border", "1px solid white" )
    , ( "border-radius", "3px" )
    , ( "padding", "10px" )
    , ( "margin-top", "20px" )
    , ( "letter-spacing", "-1px" )
    ]
