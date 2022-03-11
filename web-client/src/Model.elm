module Model exposing (..)

import Set exposing (Set)

import Board exposing (Board)
import Move exposing (Move)

type TransientError
  = RackError
  | SquareError Int Int

type alias Game =
  { board : Board
  , rack : Board.Rack
  , proposedMove : Maybe Move
  , moveError : Maybe Move.Error
  , transientError : Maybe TransientError
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
