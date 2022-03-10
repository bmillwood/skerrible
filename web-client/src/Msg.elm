module Msg exposing (..)

import Set exposing (Set)

import Key exposing (Key)
import Model

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
  | UpdateBoard Model.Board
  | UpdateRack Model.Rack
  | SetRackError Bool
  | ProposeMove (Maybe Model.Move)
  | SendMove
  | MoveResult (Result Model.MoveError ())
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
