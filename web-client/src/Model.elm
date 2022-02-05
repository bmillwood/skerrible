module Model exposing (..)

import Array exposing (Array)
import Set exposing (Set)

type alias Tile =
  { char : Char
  , score : Int
  }

type alias Rack = List Tile

type alias Square =
  { letterMult : Int
  , wordMult : Int
  , tile : Maybe Tile
  }

emptySquare : Square
emptySquare = { letterMult = 1, wordMult = 1, tile = Nothing }

type alias Board =
  { topLeft : (Int, Int)
  , squares : Array (Array Square)
  }

emptyBoard : Board
emptyBoard = { topLeft = (0, 0), squares = Array.empty }

type MoveDirection
  = MoveRight
  | MoveDown

type alias Move =
  { startPos : (Int, Int)
  , direction : MoveDirection
  , tiles : List Tile
  }

type alias Chat =
  { sender : String
  , message : String
  }

type Message
  = Joined String
  | Left String
  | Chatted Chat

type alias LoginForm =
  { endpoint : String
  , username : String
  }

type LoginState
  = NotSubmitted
  | Waiting
  | Failed

type alias PreLoginState =
  { loginState : LoginState
  , loginForm : LoginForm
  }

type alias Folks = Set String

type alias ChattingState =
  { folks : Folks
  , me : String
  , messageEntry : String
  , history : List Message
  }

type State
  = PreLogin PreLoginState
  | InGame { chat : ChattingState, board : Board, rack : Rack }

type alias Model =
  { error : Maybe String
  , state : State
  }
