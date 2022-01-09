module Model exposing (..)

import Set exposing (Set)

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
  | Chatting ChattingState

type alias Model =
  { error : Maybe String
  , state : State
  }
