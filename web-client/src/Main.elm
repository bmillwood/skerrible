module Main exposing (main)

import Browser
import Json.Decode
import Html exposing (Html)
import Set
import Task

import LocationParser
import Model exposing (Model)
import Msg exposing (Msg)
import Ports
import View

init : Json.Decode.Value -> (Model, Cmd Msg)
init flags =
  let
    { error, endpoint, username, autoLogin } = LocationParser.parseLocation flags
  in
  ( { error = error
    , state =
        Model.PreLogin
          { loginState = Model.NotSubmitted
          , loginForm = { endpoint = endpoint, username = username }
          }
    }
  , if autoLogin
    then Task.perform identity (Task.succeed (Ok (Msg.PreLogin Msg.Submit)))
    else Cmd.none
  )

view : Model -> Html Msg
view = View.view

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Model.PreLogin preLogin ->
      let
        set newPreLogin = { model | state = Model.PreLogin newPreLogin }
        failed error =
          ( { model
            | state = Model.PreLogin { preLogin | loginState = Model.Failed }
            , error = Just error
            }
          , Cmd.none
          )
        loggedIn folks =
          ( { model | state =
                Model.InGame
                  { chat =
                    { folks = folks
                    , me = preLogin.loginForm.username
                    , messageEntry = ""
                    , history = []
                    }
                  , board = Model.emptyBoard
                  }
            }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok (Msg.ReceiveMessage _) -> failed "Unexpected message before login!"
        Ok (Msg.NewFolks folks) -> loggedIn folks
        Ok (Msg.ComposeMessage _) -> failed "Can't compose message before login!"
        Ok (Msg.SendMessage _) -> failed "Can't send message before login!"
        Ok (Msg.PreLogin loginMsg) ->
          case loginMsg of
            Msg.Update newForm ->
              ( set { preLogin | loginForm = newForm }, Cmd.none )
            Msg.Submit ->
              ( set { preLogin | loginState = Model.Waiting }
              , Ports.connect { endpoint = preLogin.loginForm.endpoint }
              )
            Msg.Connected ->
              ( model
              , Ports.login { username = preLogin.loginForm.username }
              )
            Msg.Accepted folks -> loggedIn folks
            Msg.Failed error ->
              failed error
    Model.InGame ({ chat, board } as game) ->
      let
        set newChatting = { model | state = Model.InGame { game | chat = newChatting } }
        error errorMsg = ( { model | error = Just errorMsg }, Cmd.none )
      in
      case msg of
        Err errorMsg -> error (Msg.errorToString errorMsg)
        Ok (Msg.PreLogin _) -> ( model, Cmd.none )
        Ok (Msg.ReceiveMessage chatMsg) ->
          ( set { chat | history = Model.Chatted chatMsg :: chat.history }
          , Cmd.none
          )
        Ok (Msg.ComposeMessage composed) ->
          ( set { chat | messageEntry = composed }, Cmd.none )
        Ok (Msg.SendMessage message) ->
          ( set { chat | messageEntry = "" }
          , Ports.chat message
          )
        Ok (Msg.NewFolks newFolks) ->
          let
            added =
              Set.diff newFolks chat.folks
              |> Set.toList |> List.map Model.Joined
            removed =
              Set.diff chat.folks newFolks
              |> Set.toList |> List.map Model.Left
          in
          ( set { chat | history = added ++ removed ++ chat.history }
          , Cmd.none
          )



subscriptions : Model -> Sub Msg
subscriptions model = Ports.subscriptions model

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
