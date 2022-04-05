module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Json.Decode
import Html exposing (Html)
import Random
import Random.List
import Set
import Task

import Board exposing (Board)
import DictTile
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
    { error, endpoint, username, room, autoLogin }
      = LocationParser.parseLocation flags
  in
  ( { error = error
    , state =
        Model.PreLogin
          { loginState = Model.NotSubmitted
          , loginForm =
              { endpoint = endpoint
              , username = username
              , roomAction =
                  case room of
                    Nothing -> Model.MakeNewRoom
                    Just _ -> Model.JoinRoom
              , roomCode = Maybe.withDefault "" room
              , roomSettings =
                  { noBoardMultipliers = False
                  , turnEnforcement = Model.NoEnforcement
                  }
              }
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
        loggedIn roomCode =
          ( { model | state =
                Model.InGame
                  { chat =
                      { folks = Set.empty
                      , me = preLogin.loginForm.username
                      , messageEntry = ""
                      , history = []
                      }
                  , game =
                      { board = Board.empty
                      , tileData = DictTile.empty
                      , scores = Dict.empty
                      , rack = []
                      , transientError = Nothing
                      , proposedMove = Nothing
                      , moveError = Nothing
                      }
                  , roomCode = roomCode
                  }
            }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok Msg.ClearError -> ({ model | error = Nothing }, Cmd.none)
        Ok Msg.ClearMoveError -> (model, Cmd.none)
        Ok Msg.DoNothing -> (model, Cmd.none)
        Ok (Msg.UpdateRoomCode code) -> loggedIn code
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
              , Ports.login preLogin.loginForm
              )
            Msg.Failed error ->
              failed error
        Ok other -> failed "Ingame-only message outside of game"
    Model.InGame ({ chat, game } as inGame) ->
      let
        setChat newChat = { model | state = Model.InGame { inGame | chat = newChat } }
        setGame newGame = { model | state = Model.InGame { inGame | game = newGame } }
        error errorMsg = ( { model | error = Just errorMsg }, Cmd.none )
      in
      case msg of
        Err errorMsg -> error (Msg.errorToString errorMsg)
        Ok (Msg.PreLogin _) -> error "Pre-login message after login"
        Ok Msg.ClearError -> ( { model | error = Nothing }, Cmd.none )
        Ok Msg.DoNothing -> ( model, Cmd.none )
        Ok (Msg.UpdateRoomCode code) ->
          ( { model | state = Model.InGame { inGame | roomCode = code } }, Cmd.none )
        Ok (Msg.ComposeMessage composed) ->
          ( setChat { chat | messageEntry = composed }, Cmd.none )
        Ok (Msg.SendMessage message) ->
          ( setChat { chat | messageEntry = "" }
          , Ports.chat message
          )
        Ok (Msg.ReceiveChatMessage chatMsg) ->
          ( setChat { chat | history = Model.Chatted chatMsg :: chat.history }
          , Cmd.none
          )
        Ok (Msg.ReceiveMove moveReport) ->
          ( setChat { chat | history = Model.PlayerMoved moveReport :: chat.history }
          , Cmd.none
          )
        Ok (Msg.ReceiveUndone by) ->
          ( setChat { chat | history = Model.PlayerUndo by :: chat.history }
          , Cmd.none
          )
        Ok Msg.GameOver ->
          ( setChat { chat | history = Model.GameOver :: chat.history }
          , Cmd.none
          )
        Ok (Msg.UpdateBoard newBoard) ->
          ( setGame { game | board = newBoard }, Cmd.none )
        Ok (Msg.UpdateTileData tileData) ->
          ( setGame { game | tileData = tileData }, Cmd.none )
        Ok (Msg.UpdateRack newRack) ->
          ( setGame { game | rack = newRack }, Cmd.none )
        Ok (Msg.ShuffleRack Nothing) ->
          -- Generate a list of indices instead of a shuffled rack to avoid
          -- overwriting in-flight racks from the server.
          ( model
          , Random.generate
              (Ok << Msg.ShuffleRack << Just)
              (Random.List.shuffle (List.indexedMap (\i _ -> i) game.rack))
          )
        Ok (Msg.ShuffleRack (Just indices)) ->
          let
            allIndicesPresent =
              List.all
                (\i -> List.member i indices)
                (List.indexedMap (\i _ -> i) game.rack)
            newRack = List.filterMap (\i -> List.head (List.drop i game.rack)) indices
          in
          if allIndicesPresent
          then ( setGame { game | rack = newRack }, Cmd.none )
          else
            -- Ignoring is fine. You can just click the button again.
            ( model, Cmd.none )
        Ok (Msg.SetTransientError newTransientError) ->
          ( setGame { game | transientError = newTransientError }, Cmd.none )
        Ok (Msg.ProposeMove move) ->
          ( setGame { game | proposedMove = move }, Cmd.none )
        Ok Msg.SendMove ->
          case game.proposedMove of
            Nothing -> ( model, Cmd.none )
            Just move -> ( model, Ports.sendMove move )
        Ok Msg.SendUndo -> ( model, Ports.sendUndo )
        Ok (Msg.MoveResult (Err moveError)) ->
          ( setGame { game | moveError = Just moveError }, Cmd.none )
        Ok (Msg.MoveResult (Ok ())) ->
          ( setGame { game | moveError = Nothing, proposedMove = Nothing }, Cmd.none )
        Ok Msg.ClearMoveError ->
          ( setGame { game | moveError = Nothing }, Cmd.none )
        Ok (Msg.UpdateScores newScores) ->
          let
            newFolks = Set.fromList (Dict.keys newScores)
            added =
              Set.diff newFolks chat.folks
              |> Set.toList |> List.map Model.Joined
            removed =
              Set.diff chat.folks newFolks
              |> Set.toList |> List.map Model.Left
          in
          ( { model
            | state = Model.InGame
                { inGame
                | chat =
                  { chat
                  | folks = newFolks
                  , history = added ++ removed ++ chat.history
                  }
                , game = { game | scores = newScores }
                }
            }
          , Cmd.none
          )

updateMoveWithKey : Board -> Board.Rack -> Move -> Key -> Msg.OkMsg
updateMoveWithKey board rack move key =
  case key of
    Key.Backspace ->
      Msg.ProposeMove (Just { move | tiles = List.take (List.length move.tiles - 1) move.tiles })
    Key.Enter -> Msg.SendMove
    Key.Char c ->
      let
        (i, j) = Move.nextPos move
        addTile moveTile =
          Msg.ProposeMove (Just { move | tiles = move.tiles ++ [moveTile] })
      in
      case Board.get i j board of
        Nothing -> Msg.SetTransientError (Just Model.BoardError)
        Just sq ->
          case sq.tile of
            Nothing ->
              case Board.tileOfChar c of
                Nothing -> Msg.DoNothing
                Just tile ->
                  if List.member tile (Move.remainingRack move rack)
                  then addTile (Move.PlaceTile tile)
                  else Msg.SetTransientError (Just Model.RackError)
            Just tile ->
              if Board.tileToChar tile == c
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
