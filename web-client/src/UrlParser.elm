module UrlParser exposing (..)

import Dict
import Result exposing (Result)
import Tuple

import Url exposing (Url)
import Url.Parser
import Url.Parser.Query

import Model

type alias Flags =
  { error : Maybe String
  , endpoint : String
  , username : String
  , room : Maybe String
  , autoLogin : Bool
  , turns : Model.TurnEnforcement
  , muted : Bool
  }

defaults : Flags
defaults =
  { error = Nothing
  , endpoint = "ws://localhost:4170"
  , username = ""
  , room = Nothing
  , autoLogin = False
  , turns = Model.LetPlayersChoose
  , muted = False
  }

type alias QueryParams =
  { username : Maybe String
  , room : Maybe String
  , autoLogin : Maybe Bool
  , turns : Maybe Model.TurnEnforcement
  , muted : Maybe Bool
  }

queryParser =
  let
    bool name =
      Url.Parser.Query.enum name (Dict.fromList [ ("true", True), ("false", False) ])
    turns =
      Url.Parser.Query.enum
        "turns"
        (Dict.fromList
          [ ( "chaos", Model.NoEnforcement )
          , ( "follow", Model.LetPlayersChoose )
          ]
        )
  in
  Url.Parser.Query.map5
    QueryParams
    (Url.Parser.Query.string "username")
    (Url.Parser.Query.string "room")
    (bool "autoLogin")
    turns
    (bool "muted")

protocolWorkaround : { replaceWith : String } -> String -> (String, String)
protocolWorkaround { replaceWith } url =
  case String.split ":" url of
    [] -> ("", url)
    protocol :: _ ->
      (protocol, replaceWith ++ String.dropLeft (String.length protocol) url)

parseUrl : Url -> Flags
parseUrl url =
  let
    websocketProtocol =
      if url.protocol == Url.Https
      then "wss"
      else "ws"

    endpoint =
      { url | query = Nothing, fragment = Nothing }
      |> Url.toString
      |> protocolWorkaround { replaceWith = websocketProtocol }
      |> Tuple.second

    handleError result =
      case result of
        Err e -> { defaults | error = Just e }
        Ok flags -> flags
  in
  Url.Parser.parse (Url.Parser.query queryParser) { url | path = "" }
  |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url)
  |> Result.map (\{ username, room, autoLogin, turns, muted } ->
      { error = Nothing
      , username = Maybe.withDefault defaults.username username
      , room = room
      , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
      , endpoint = endpoint
      , turns = Maybe.withDefault defaults.turns turns
      , muted = Maybe.withDefault defaults.muted muted
      }
    )
  |> handleError
