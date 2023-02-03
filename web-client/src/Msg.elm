module Msg exposing (..)

import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Url exposing (Url)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Key exposing (Key)
import Model
import Move exposing (Move)

type GlobalMsg
  = SetError (Maybe String)
  | BlurById String
  | UrlRequest Browser.UrlRequest
  | UrlChange Url

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | EnterRoom { code : String }
  | NoSuchRoom

type ProposalUpdate
  = ProposeTile Board.Tile
  | UnproposeLast
  | CancelProposal
  | SubmitProposal

type RoomMsg
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
  | SendStartNewGame
  | MoveResult (Result Move.Error ())
  | GameOver
  | NewGameStarted { by : String }
  | ClearMoveError
  | SetTransientError (Maybe Model.TransientError)
  | SetHelpVisible Bool
  | SetUseKeysForGame Bool

type OneMsg
  = Global GlobalMsg
  | PreLogin LoginFormMsg
  | InRoom RoomMsg

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
