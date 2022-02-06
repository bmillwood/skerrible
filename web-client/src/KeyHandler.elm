module KeyHandler exposing (..)

import Dict
import Html
import Html.Events as Events
import Json.Decode

import Model
import Msg

charOfString : String -> Json.Decode.Decoder Char
charOfString s =
  case String.uncons s of
    Nothing -> Json.Decode.fail "JSON char: empty string"
    Just (c, r) ->
      if not (String.isEmpty r)
      then Json.Decode.fail <|
        "JSON char: String " ++ Debug.toString s ++ " is not length 1"
      else Json.Decode.succeed c

decodeChar : Json.Decode.Decoder Char
decodeChar =
  Json.Decode.string
  |> Json.Decode.andThen charOfString

type Key
  = Letter Char
  | Backspace
  | Enter
  | Escape
  | Other

decodeKey : Json.Decode.Decoder Key
decodeKey =
  let
    ofChar c =
      if Char.isAlpha c
      then Letter (Char.toUpper c)
      else Other
  in
  Json.Decode.field "key" Json.Decode.string
  |> Json.Decode.andThen (\s ->
      if s == "Backspace"
      then Json.Decode.succeed Backspace
      else if s == "Enter"
      then Json.Decode.succeed Enter
      else if s == "Escape"
      then Json.Decode.succeed Escape
      else Json.Decode.map ofChar (charOfString s)
    )

updateMoveWithKey : Model.Rack -> Model.Move -> Key -> Msg.OkMsg
updateMoveWithKey rack move key =
  let
    values = Dict.fromList (List.map (\tile -> (tile.char, tile.score)) rack)
  in
  case key of
    Backspace ->
      Msg.ProposeMove (Just { move | tiles = List.take (List.length move.tiles - 1) move.tiles })
    Enter -> Msg.SendMove
    Letter c ->
      case Dict.get c values of
        Nothing -> Msg.DoNothing
        Just v -> Msg.ProposeMove (Just { move | tiles = move.tiles ++ [{ char = c, score = v }] })
    Escape ->
      if List.isEmpty move.tiles
      then Msg.ProposeMove Nothing
      else Msg.DoNothing
    Other -> Msg.DoNothing

handleKey : Model.Model -> Json.Decode.Decoder Msg.Msg
handleKey model =
  case model.state of
    Model.PreLogin _ -> Json.Decode.succeed (Ok Msg.DoNothing)
    Model.InGame { game } ->
      case game.proposedMove of
        Nothing -> Json.Decode.succeed (Ok Msg.DoNothing)
        Just move ->
          Json.Decode.map
            (Ok << updateMoveWithKey game.rack move)
            decodeKey
