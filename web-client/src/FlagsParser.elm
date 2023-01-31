module FlagsParser exposing (..)

import Dict
import Json.Decode
import Result exposing (Result)
import Tuple

import Url
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
  }

defaults : Flags
defaults =
  { error = Nothing
  , endpoint = "ws://localhost:4170"
  , username = ""
  , room = Nothing
  , autoLogin = False
  , turns = Model.LetPlayersChoose
  }

type alias QueryParams =
  { username : Maybe String
  , room : Maybe String
  , autoLogin : Maybe Bool
  , turns : Maybe Model.TurnEnforcement
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
  Url.Parser.Query.map4
    QueryParams
    (Url.Parser.Query.string "username")
    (Url.Parser.Query.string "room")
    (bool "autoLogin")
    turns

protocolWorkaround : { replaceWith : String } -> String -> (String, String)
protocolWorkaround { replaceWith } url =
  case String.split ":" url of
    [] -> ("", url)
    protocol :: _ ->
      (protocol, replaceWith ++ String.dropLeft (String.length protocol) url)

parseHref : String -> Result String Flags
parseHref href =
  let
    (originalProtocol, replaced) =
      protocolWorkaround { replaceWith = "https" } href

    websocketProtocol =
      if originalProtocol == "https"
      then "wss"
      else "ws"

    endpointForUrl url =
      { url | query = Nothing, fragment = Nothing }
      |> Url.toString
      |> protocolWorkaround { replaceWith = websocketProtocol }
      |> Tuple.second
  in
  Url.fromString replaced
  |> Result.fromMaybe ("Url parsing failed on: " ++ href)
  |> Result.andThen (\url ->
      Url.Parser.parse (Url.Parser.query queryParser) { url | path = "" }
      |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url)
      |> Result.map (\{ username, room, autoLogin, turns } ->
          { error = Nothing
          , username = Maybe.withDefault defaults.username username
          , room = room
          , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
          , endpoint = endpointForUrl url
          , turns = Maybe.withDefault defaults.turns turns
          }
        )
    )

parseFlags : Json.Decode.Value -> Flags
parseFlags rawFlags =
  let
    parsedUrl =
      Json.Decode.decodeValue
        (Json.Decode.at ["location", "href"] Json.Decode.string)
        rawFlags
      |> Result.mapError Json.Decode.errorToString
      |> Result.andThen parseHref
  in
  case parsedUrl of
    Err msg -> { defaults | error = Just msg }
    Ok flags -> flags
