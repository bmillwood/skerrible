module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Json.Decode
import Html exposing (Html)
import Set
import Task

import Board exposing (Board)
import Key exposing (Key)
import LocationParser
import Model exposing (Model)
import Move exposing (Move)
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
                      { board = Board.empty
                      , rack = []
                      , transientError = Nothing
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
        Ok (Msg.SetTransientError _) ->
          failed "Unexpected transient error before login!"
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
        Ok (Msg.SetTransientError newTransientError) ->
          ( setGame { game | transientError = newTransientError }, Cmd.none )
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

updateMoveWithKey : Board -> Board.Rack -> Move -> Key -> Msg.OkMsg
updateMoveWithKey board rack move key =
  case key of
    Key.Backspace ->
      Msg.ProposeMove (Just { move | tiles = List.take (List.length move.tiles - 1) move.tiles })
    Key.Enter -> Msg.SendMove
    Key.Letter c ->
      let
        (i, j) = Move.nextPos move
        addTile moveTile =
          Msg.ProposeMove (Just { move | tiles = move.tiles ++ [moveTile] })
      in
      case Board.getTile i j board of
        Nothing ->
          case Dict.get c (Board.rackByChar rack) of
            Nothing -> Msg.SetTransientError (Just Model.RackError)
            Just countByScore ->
              case List.head (List.reverse (Dict.keys countByScore)) of
                Nothing -> Msg.SetTransientError (Just Model.RackError)
                Just v -> addTile (Move.PlaceTile { char = c, score = v })
        Just tile ->
          if tile.char == c
          then addTile Move.UseBoard
          else Msg.SetTransientError (Just (Model.SquareError i j))
    Key.Escape ->
      if List.isEmpty move.tiles
      then Msg.ProposeMove Nothing
      else Msg.DoNothing
    Key.Other -> Msg.DoNothing

handleKey : Model.Game -> Json.Decode.Decoder Msg.OkMsg
handleKey game =
  case game.proposedMove of
    Nothing -> Json.Decode.succeed Msg.DoNothing
    Just move ->
      Json.Decode.map
        (updateMoveWithKey game.board game.rack move)
        Key.decodeKey

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    ifPlaying decoderOfGame =
      case model.state of
        Model.PreLogin _ -> Json.Decode.succeed (Ok Msg.DoNothing)
        Model.InGame { game } -> Json.Decode.map Ok (decoderOfGame game)
  in
  Sub.batch
    [ Ports.subscriptions model
    , Browser.Events.onKeyDown (ifPlaying handleKey)
    , Browser.Events.onKeyUp
        (ifPlaying (\_ -> Json.Decode.succeed (Msg.SetTransientError Nothing)))
    ]

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
