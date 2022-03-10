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
  { top : Int
  , left : Int
  , squares : Array (Array Square)
  }

emptyBoard : Board
emptyBoard = { top = 0, left = 0, squares = Array.empty }

type MoveDirection
  = MoveRight
  | MoveDown

type alias Move =
  { startRow : Int
  , startCol : Int
  , direction : MoveDirection
  , tiles : List Tile
  }

type MoveError
  = NotPlaying
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | YouDoNotHave (List Tile)
  | DoesNotConnect
  | NotAWord (List Move)

type alias Game =
  { board : Board
  , rack : Rack
  , rackError : Bool
  , proposedMove : Maybe Move
  , moveError : Maybe MoveError
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
  | InGame { chat : ChattingState, game : Game }

type alias Model =
  { error : Maybe String
  , state : State
  }
