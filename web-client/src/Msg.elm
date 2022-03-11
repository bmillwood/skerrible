module Msg exposing (..)

import Set exposing (Set)

import Board exposing (Board)
import Key exposing (Key)
import Model
import Move exposing (Move)

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | Accepted Model.Folks
  | Failed String

type OkMsg
  = DoNothing
  | PreLogin LoginFormMsg
  | ComposeMessage String
  | SendMessage String
  | ReceiveMessage Model.Chat
  | NewFolks Model.Folks
  | UpdateBoard Board
  | UpdateRack Board.Rack
  | SetRackError Bool
  | ProposeMove (Maybe Move)
  | SendMove
  | MoveResult (Result Move.Error ())
  | ClearMoveError

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
