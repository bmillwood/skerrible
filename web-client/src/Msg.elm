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
  | ReceiveChatMessage Model.Chat
  | ReceiveMove Model.MoveReport
  | UpdateScores (Dict String Int)
  | UpdateBoard Board
  | UpdateTileData (DictTile Board.TileData)
  | UpdateRack Board.Rack
  | ShuffleRack (Maybe (List Int))
  | ProposeMove (Maybe Move)
  | SendMove
  | MoveResult (Result Move.Error ())
  | ClearMoveError
  | SetTransientError (Maybe Model.TransientError)

type Error
  = ServerDisconnected
  | ServerProtocolError String
  | DriverProtocolError String
  | ClientError String

errorToString : Error -> String
errorToString error =
  case error of
    ServerDisconnected -> "Server disconnected"
    ServerProtocolError s -> "Server protocol error: " ++ s
    DriverProtocolError s -> "Driver protocol error: " ++ s
    ClientError s -> "Client error: " ++ s

type alias Msg = Result Error OkMsg
