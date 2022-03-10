module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Json.Decode
import Html exposing (Html)
import Set
import Task

import Key exposing (Key)
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
                  , game =
                      { board = Model.emptyBoard
                      , rack = []
                      , proposedMove = Nothing
                      , moveError = Nothing
                      }
                  }
            }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok Msg.DoNothing -> (model, Cmd.none)
        Ok (Msg.ComposeMessage _) -> failed "Can't compose message before login!"
        Ok (Msg.SendMessage _) -> failed "Can't send message before login!"
        Ok (Msg.ReceiveMessage _) -> failed "Unexpected message before login!"
        Ok (Msg.NewFolks folks) -> loggedIn folks
        Ok (Msg.UpdateBoard _) -> failed "Unexpected board before login!"
        Ok (Msg.UpdateRack _) -> failed "Unexpected rack before login!"
        Ok (Msg.ProposeMove _) -> failed "Can't propose move before login!"
        Ok Msg.SendMove -> failed "Can't send move before login!"
        Ok (Msg.MoveResult _) -> failed "Unexpected move result before login!"
        Ok Msg.ClearMoveError -> (model, Cmd.none)
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
    Model.InGame ({ chat, game } as inGame) ->
      let
        setChat newChat = { model | state = Model.InGame { inGame | chat = newChat } }
        setGame newGame = { model | state = Model.InGame { inGame | game = newGame } }
        error errorMsg = ( { model | error = Just errorMsg }, Cmd.none )
      in
      case msg of
        Err errorMsg -> error (Msg.errorToString errorMsg)
        Ok Msg.DoNothing -> (model, Cmd.none)
        Ok (Msg.PreLogin _) -> ( model, Cmd.none )
        Ok (Msg.ComposeMessage composed) ->
          ( setChat { chat | messageEntry = composed }, Cmd.none )
        Ok (Msg.SendMessage message) ->
          ( setChat { chat | messageEntry = "" }
          , Ports.chat message
          )
        Ok (Msg.ReceiveMessage chatMsg) ->
          ( setChat { chat | history = Model.Chatted chatMsg :: chat.history }
          , Cmd.none
          )
        Ok (Msg.UpdateBoard newBoard) ->
          ( setGame { game | board = newBoard }, Cmd.none )
        Ok (Msg.UpdateRack newRack) ->
          ( setGame { game | rack = newRack }, Cmd.none )
        Ok (Msg.ProposeMove move) ->
          ( setGame { game | proposedMove = move }, Cmd.none )
        Ok Msg.SendMove ->
          case game.proposedMove of
            Nothing -> ( model, Cmd.none )
            Just move -> ( model, Ports.sendMove move )
        Ok (Msg.MoveResult (Err moveError)) ->
          ( setGame { game | moveError = Just moveError }, Cmd.none )
        Ok (Msg.MoveResult (Ok ())) ->
          ( setGame { game | moveError = Nothing, proposedMove = Nothing }, Cmd.none )
        Ok Msg.ClearMoveError ->
          ( setGame { game | moveError = Nothing }, Cmd.none )
        Ok (Msg.NewFolks newFolks) ->
          let
            added =
              Set.diff newFolks chat.folks
              |> Set.toList |> List.map Model.Joined
            removed =
              Set.diff chat.folks newFolks
              |> Set.toList |> List.map Model.Left
          in
          ( setChat { chat | history = added ++ removed ++ chat.history }
          , Cmd.none
          )

updateMoveWithKey : Model.Rack -> Model.Move -> Key -> Msg.OkMsg
updateMoveWithKey rack move key =
  let
    values = Dict.fromList (List.map (\tile -> (tile.char, tile.score)) rack)
  in
  case key of
    Key.Backspace ->
      Msg.ProposeMove (Just { move | tiles = List.take (List.length move.tiles - 1) move.tiles })
    Key.Enter -> Msg.SendMove
    Key.Letter c ->
      case Dict.get c values of
        Nothing -> Msg.DoNothing
        Just v -> Msg.ProposeMove (Just { move | tiles = move.tiles ++ [{ char = c, score = v }] })
    Key.Escape ->
      if List.isEmpty move.tiles
      then Msg.ProposeMove Nothing
      else Msg.DoNothing
    Key.Other -> Msg.DoNothing

handleKey : Model.Model -> Json.Decode.Decoder Msg.Msg
handleKey model =
  case model.state of
    Model.PreLogin _ -> Json.Decode.succeed (Ok Msg.DoNothing)
    Model.InGame { game } ->
      case game.proposedMove of
        Nothing -> Json.Decode.succeed (Ok Msg.DoNothing)
        Just move ->
          Json.Decode.map
            (Ok << updateMoveWithKey game.rack move)
            Key.decodeKey

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Ports.subscriptions model
    , Browser.Events.onKeyDown (handleKey model)
    ]

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
