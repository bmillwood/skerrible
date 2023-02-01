module Msg exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Key exposing (Key)
import Model
import Move exposing (Move)

type GlobalMsg
  = SetError (Maybe String)
  | BlurById String

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | UpdateRoomCode String
  | NoSuchRoom

type ProposalUpdate
  = ProposeTile Board.Tile
  | UnproposeLast
  | CancelProposal
  | SubmitProposal

type GameMsg
  = ComposeMessage String
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
  | SetHelpVisible Bool

type OneMsg
  = Global GlobalMsg
  | PreLogin LoginFormMsg
  | InGame GameMsg

error : String -> OneMsg
error e = Global (SetError (Just e))

serverDisconnected : OneMsg
serverDisconnected = error "Server disconnected"

serverProtocolError : String -> OneMsg
serverProtocolError s = error ("Server protocol error: " ++ s)

driverProtocolError : String -> OneMsg
driverProtocolError s = error ("Driver protocol error: " ++ s)

clientError : String -> OneMsg
clientError s = error ("Client error: " ++ s)

type alias Msg = List OneMsg

doNothing : Msg
doNothing = []
