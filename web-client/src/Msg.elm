module Msg exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Key exposing (Key)
import Model
import Move exposing (Move)

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | Failed String

type OkMsg
  = DoNothing
  | ClearError
  | PreLogin LoginFormMsg
  | ComposeMessage String
  | SendMessage String
  | ReceiveMessage Model.Chat
  | UpdateScores (Dict String Int)
  | UpdateBoard Board
  | UpdateTileData (DictTile Board.TileData)
  | UpdateRack Board.Rack
  | ProposeMove (Maybe Move)
  | SendMove
  | MoveResult (Result Move.Error Int)
  | ClearMoveError
  | SetTransientError (Maybe Model.TransientError)

type Error
  = ServerDisconnected
  | ServerProtocolError String
  | DriverProtocolError String

errorToString : Error -> String
errorToString error =
  case error of
    ServerDisconnected -> "Server disconnected"
    ServerProtocolError s -> "Server protocol error: " ++ s
    DriverProtocolError s -> "Driver protocol error: " ++ s

type alias Msg = Result Error OkMsg
