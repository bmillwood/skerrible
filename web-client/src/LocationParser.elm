module LocationParser exposing (..)

import Dict
import Json.Decode
import Result
import Tuple

import Url
import Url.Parser
import Url.Parser.Query

type alias QueryParams =
  { username : Maybe String
  , room : Maybe String
  , autoLogin : Maybe Bool
  }

queryParser =
  let
    bool name =
      Url.Parser.Query.enum name (Dict.fromList [ ("true", True), ("false", False) ])
  in
  Url.Parser.Query.map3
    QueryParams
    (Url.Parser.Query.string "username")
    (Url.Parser.Query.string "room")
    (bool "autoLogin")

defaults =
  { error = Nothing
  , endpoint = "ws://localhost:4170"
  , username = ""
  , room = Nothing
  , autoLogin = False
  }

protocolWorkaround : { replaceWith : String } -> String -> (String, String)
protocolWorkaround { replaceWith } url =
  case String.split ":" url of
    [] -> ("", url)
    protocol :: _ ->
      (protocol, replaceWith ++ String.dropLeft (String.length protocol) url)

parseLocation
  :  Json.Decode.Value
  -> { error : Maybe String
     , endpoint : String
     , username : String
     , room : Maybe String
     , autoLogin : Bool
     }
parseLocation flags =
  let
    parsedUrl =
      Json.Decode.decodeValue (Json.Decode.at ["location", "href"] Json.Decode.string) flags
      |> Result.mapError Json.Decode.errorToString
      |> Result.andThen (\href ->
          let
            (originalProtocol, replaced) =
              protocolWorkaround { replaceWith = "https" } href
          in
          Url.fromString replaced
          |> Result.fromMaybe ("Url parsing failed on: " ++ href)
          |> Result.map (\url -> (originalProtocol, url))
        )
      |> Result.andThen (\(originalProtocol, url) ->
          Url.Parser.parse (Url.Parser.query queryParser) { url | path = "" }
          |> Maybe.map (\query -> (originalProtocol, url, query))
          |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url)
        )
  in
  case parsedUrl of
    Err msg ->
      { defaults | error = Just msg }
    Ok (originalProtocol, url, { username, room, autoLogin }) ->
      let
        websocketProtocol =
          if originalProtocol == "https"
          then "wss"
          else "ws"
        endpoint =
          { url | query = Nothing, fragment = Nothing }
          |> Url.toString
          |> protocolWorkaround { replaceWith = websocketProtocol }
          |> Tuple.second
      in
      { defaults
      | username = Maybe.withDefault defaults.username username
      , room = room
      , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
      , endpoint = endpoint
      }
