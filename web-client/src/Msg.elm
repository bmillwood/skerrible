module Msg exposing (..)

import Set exposing (Set)

import Model

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | Accepted Model.Folks
  | Failed String

type OkMsg
  = PreLogin LoginFormMsg
  | ComposeMessage String
  | SendMessage String
  | ReceiveMessage Model.Chat
  | NewFolks Model.Folks
  | UpdateBoard Model.Board
  | UpdateRack Model.Rack
  | SendMove Model.Move

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
