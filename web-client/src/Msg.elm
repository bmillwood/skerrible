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
  | NoSuchRoom

type ProposalUpdate
  = ProposeTile Board.Tile
  | UnproposeLast
  | CancelProposal
  | SubmitProposal

type OkMsg
  = Many (List OkMsg)
  | ClearError
  | PreLogin LoginFormMsg
  | UpdateRoomCode String
  | ComposeMessage String
  | SendMessage String
  | ReceiveChatMessage Model.Chat
  | SendJoin
  | ReceiveMove { player : String, moveReport : Model.MoveReport }
  | UpdatePeople (Set String)
  | UpdateScores (Dict String Int)
  | UpdateBoard Board
  | UpdateTileData (DictTile Board.TileData)
  | UpdateRack Board.Rack
  | ShuffleRack (Maybe (List Int))
  | UpdateProposal ProposalUpdate
  | Propose (Maybe Move.Proposal)
  | SendProposal
  | SendPass
  | SendUndo
  | MoveResult (Result Move.Error ())
  | GameOver
  | ClearMoveError
  | SetTransientError (Maybe Model.TransientError)
  | BlurById String
  | SetHelpVisible Bool

doNothing : OkMsg
doNothing = Many []

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
