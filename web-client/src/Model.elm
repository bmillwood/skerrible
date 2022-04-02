module Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Move exposing (Move)

type TransientError
  = RackError
  | SquareError Int Int
  | BoardError

type alias Game =
  { board : Board
  , tileData : DictTile Board.TileData
  , scores : Dict String Int
  , rack : Board.Rack
  , proposedMove : Maybe Move
  , moveError : Maybe Move.Error
  , transientError : Maybe TransientError
  }

type alias Chat =
  { sender : String
  , message : String
  }

type alias MoveReport =
  { madeBy : String
  , words : List String
  , score : Int
  }

type Message
  = Joined String
  | Left String
  | Chatted Chat
  | PlayerMoved MoveReport

type RoomSpec
  = JoinRoom String
  | MakeNewRoom { noBoardMultipliers : Bool }

type alias LoginForm =
  { endpoint : String
  , username : String
  , roomSpec : RoomSpec
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

type State
  = PreLogin PreLoginState
  | InGame { roomCode : String, chat : ChattingState, game : Game }

type alias Model =
  { error : Maybe String
  , state : State
  }
