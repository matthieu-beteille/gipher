module Login where

import ElmFire exposing (childAdded, noOrder)
import ElmFire.Auth exposing (..)
import Task
import Effects exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onMouseUp )

type alias User =
  ({ uid: String
  , token: String
  , displayName: String } )

type alias Model =
    Maybe ( User )

type Action
  = Login (Maybe Authentication)
  | Logout
  | NoOp

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
                            |> Task.map (always NoOp)
                            |> Effects.task
          in
            ( userObject, effects )

        Nothing ->
          ( model, login root )

    Logout ->
      ( model, Effects.none )

    NoOp ->
      ( model, Effects.none )

decodeDisplayName: Decoder String
decodeDisplayName =
    "displayName" := string

getUserFromAuth: Authentication -> User
getUserFromAuth auth =
  User auth.uid auth.token (Result.withDefault "" (decodeValue decodeDisplayName auth.specifics))

loginView: Signal.Address Action -> Model -> Html
loginView address model =
  div [] [ h1 [titleStyle] [text "Gipher"]
          , div [btnStyle] [ a [onClick address (Login Nothing)] [text "Login with Facebook"] ] ]

login: ElmFire.Location -> Effects Action
login loc =
  authenticate loc [] (withOAuthPopup "facebook")
    |> Task.toMaybe
    |> Task.map Login
    |> Effects.task

titleStyle: Attribute
titleStyle =
  style [ ("color", "white")
        , ("text-align", "center")
        , ("margin-bottom", "50px")
        , ("font-size", "2.5em") ]

btnStyle : Attribute
btnStyle =
  style [ ("font-size", "20px")
        , ("color", "white")
        , ("cursor", "pointer")
        , ("display", "inline-block")
        , ("width", "100px")
        , ("text-align", "center")
        , ("border", "1px solid white")
        , ("border-radius", "3px")
        , ("padding", "10px") ]
