module Model exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Move exposing (Move)

type TransientError
  = RackError
  | SquareError Int Int
  | BoardError

type alias Playing =
  { rack : Board.Rack
  , proposal : Maybe Move.Proposal
  }

type alias PlayerData =
  { score : Int
  , canMove : Bool
  }

type alias Game =
  { board : Board
  , tileData : DictTile Board.TileData
  , players : Dict String PlayerData
  , playing : Maybe Playing
  , moveError : Maybe Move.Error
  , transientError : Maybe TransientError
  , showHelp : Bool
  , useKeysForGame : Bool
  , gameOver : Bool
  }

type alias Chat =
  { sender : String
  , message : String
  }

type MoveReport
  = PlayedWord { words : List String, score : Int }
  | Exchanged Int
  | Passed
  | Undone

type Message
  = JoinedRoom String
  | LeftRoom String
  | JoinedGame String
  | Chatted Chat
  | PlayerMoved { player : String, moveReport : MoveReport, next : Set String }
  | GameOver
  | NewGameStarted { by : String }

type RoomAction
  = JoinRoom
  | MakeNewRoom

type TurnEnforcement
  = NoEnforcement
  | LetPlayersChoose

type alias RoomSettings =
  { noBoardMultipliers : Bool
  , turnEnforcement : TurnEnforcement
  }

type alias LoginForm =
  { endpoint : String
  , username : String
  , roomAction : RoomAction
  , roomCode : String
  , roomSettings : RoomSettings
  }

type LoginState
  = NotSubmitted
  | Waiting
  | Failed

type alias PreLoginState =
  { loginState : LoginState
  , loginForm : LoginForm
  }

type alias ChattingState =
  { folks : Set String
  , me : String
  , messageEntry : String
  , history : List Message
  }

type alias RoomState =
  { roomCode : String
  , chat : ChattingState
  , game : Game
  }

type State
  = PreLogin PreLoginState
  | InRoom RoomState

type alias Model =
  { navKey : Browser.Navigation.Key
  , muted : Bool
  , showAbout : Bool
  , error : Maybe String
  , state : State
  }
