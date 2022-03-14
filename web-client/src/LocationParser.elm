module LocationParser exposing (..)

import Dict
import Json.Decode
import Result

import Url
import Url.Parser
import Url.Parser.Query

type alias QueryParams = { username : Maybe String, autoLogin : Maybe Bool }

queryParser =
  let
    bool name =
      Url.Parser.Query.enum name (Dict.fromList [ ("true", True), ("false", False) ])
  in
  Url.Parser.Query.map2
    QueryParams
    (Url.Parser.Query.string "username")
    (bool "autoLogin")

defaults =
  { error = Nothing
  , endpoint = "ws://localhost:4170"
  , username = ""
  , autoLogin = False
  }

protocolWorkaround : { replaceWith : String } -> String -> String
protocolWorkaround { replaceWith } url =
  case String.split ":" url of
    [] -> url
    protocol :: _ ->
      replaceWith ++ String.dropLeft (String.length protocol) url

parseLocation : Json.Decode.Value -> { error : Maybe String, endpoint : String, username : String, autoLogin : Bool }
parseLocation flags =
  let
    parsedUrl =
      Json.Decode.decodeValue (Json.Decode.at ["location", "href"] Json.Decode.string) flags
      |> Result.mapError Json.Decode.errorToString
      |> Result.andThen (\href ->
          Url.fromString (protocolWorkaround { replaceWith = "https" } href)
          |> Result.fromMaybe ("Url parsing failed on: " ++ href)
        )
      |> Result.andThen (\url ->
          Url.Parser.parse (Url.Parser.query queryParser) { url | path = "" }
          |> Maybe.map (\query -> (url, query))
          |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url)
        )
  in
  case parsedUrl of
    Err msg ->
      { defaults | error = Just msg }
    Ok (url, { username, autoLogin }) ->
      let
        endpoint =
          { url | query = Nothing, fragment = Nothing }
          |> Url.toString
          |> protocolWorkaround { replaceWith = "ws" }
      in
      { defaults
      | username = Maybe.withDefault defaults.username username
      , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
      , endpoint = endpoint
      }
