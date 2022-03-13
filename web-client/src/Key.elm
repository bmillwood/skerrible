module Key exposing (..)

import Json.Decode

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
  = Char Char
  | Backspace
  | Enter
  | Escape
  | Other

decodeKey : Json.Decode.Decoder Key
decodeKey =
  let
    ofChar c =
      if Char.isAlpha c
      then Char (Char.toUpper c)
      else if c == ' '
      then Char ' '
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
