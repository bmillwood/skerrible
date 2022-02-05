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

type alias Board = Array (Array Square)

emptyBoard : Board
emptyBoard = Array.empty

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
