port module Ports exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Set exposing (Set)

import Board exposing (Board)
import Key
import Model
import Move exposing (Move)
import Msg exposing (Msg)

port sendToJS : Json.Encode.Value -> Cmd msg
port receiveFromJS : (Json.Decode.Value -> msg) -> Sub msg

request : { kind : String, payload : Json.Encode.Value } -> Cmd msg
request { kind, payload } =
  Json.Encode.object
    [ ("kind", Json.Encode.string kind)
    , ("payload", payload)
    ]
  |> sendToJS

connect : { endpoint : String } -> Cmd msg
connect { endpoint } = request { kind = "connect", payload = Json.Encode.string endpoint }

send : Json.Encode.Value -> Cmd msg
send value = request { kind = "send", payload = value }

withTag : String -> List (String, Json.Encode.Value) -> Json.Encode.Value
withTag tag fields =
  Json.Encode.object (("tag", Json.Encode.string tag) :: fields)

login : { username : String } -> Cmd msg
login { username } =
  withTag "LoginRequest" [("loginRequestName", Json.Encode.string username)]
  |> send

chat : String -> Cmd msg
chat message =
  withTag "Chat" [("msgToSend", Json.Encode.string message)]
  |> send

encodeDirection : Move.Direction -> Json.Encode.Value
encodeDirection d =
  Json.Encode.string (
    case d of
      Move.Right -> "MoveRight"
      Move.Down -> "MoveDown"
  )

encodeTile : Board.Tile -> Json.Encode.Value
encodeTile { char, score } =
  Json.Encode.object
    [ ( "tileChar", Json.Encode.string (String.fromChar char) )
    , ( "tileScore", Json.Encode.int score )
    ]

encodeMoveTile : Move.Tile -> Json.Encode.Value
encodeMoveTile moveTile =
  case moveTile of
    Move.PlaceTile tile -> withTag "PlaceTile" [("contents", encodeTile tile)]
    Move.UseBoard -> withTag "UseBoard" []

sendMove : Move -> Cmd msg
sendMove { startRow, startCol, direction, tiles } =
  withTag "MakeMove"
    [ ( "contents"
      , Json.Encode.object
          [ ( "startPos", Json.Encode.list Json.Encode.int [startRow, startCol] )
          , ( "direction", encodeDirection direction )
          , ( "tiles", Json.Encode.list encodeMoveTile tiles )
          ]
      )
    ]
  |> send

encodeNullable : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNullable encode m = Maybe.withDefault Json.Encode.null (Maybe.map encode m)

type ConnectionStatus
  = Connected
  | Disconnected

plainVariant : { name : String } -> List (String, a) -> Json.Decode.Decoder a
plainVariant { name } values =
  let
    dict = Dict.fromList values
  in
  Json.Decode.string
  |> Json.Decode.andThen (\k ->
       case Dict.get k dict of
         Just v -> Json.Decode.succeed v
         Nothing -> Json.Decode.fail (name ++ ": unknown variant: " ++ k))

type VariantSpec a
  = Plain a
  | WithFieldsInline (Json.Decode.Decoder a)
  | WithContents (Json.Decode.Decoder a)

variant : { name : String } -> List (String, VariantSpec a) -> Json.Decode.Decoder a
variant { name } values =
  let
    dict = Dict.fromList values
  in
  Json.Decode.field "tag" Json.Decode.string
  |> Json.Decode.andThen (\k ->
       case Dict.get k dict of
         Nothing -> Json.Decode.fail (name ++ ": unknown tag: " ++ k)
         Just (Plain v) -> Json.Decode.succeed v
         Just (WithFieldsInline decoder) -> decoder
         Just (WithContents decoder) -> Json.Decode.field "contents" decoder)

type ServerMsg
  = Folks Model.Folks
  | Message Model.Chat
  | UpdateBoard Board.Board
  | UpdateRack Board.Rack
  | MoveResult (Result Move.Error ())

type FromJS
  = ServerStatus ConnectionStatus
  | FromServer ServerMsg

connectionStatus : Json.Decode.Decoder ConnectionStatus
connectionStatus =
  plainVariant
    { name = "connectionStatus" }
    [ ("connected", Connected)
    , ("disconnected", Disconnected)
    ]

decodeTile : Json.Decode.Decoder Board.Tile
decodeTile =
  Json.Decode.map2
    Board.Tile
    (Json.Decode.field "tileChar" Key.decodeChar)
    (Json.Decode.field "tileScore" Json.Decode.int)

rack : Json.Decode.Decoder Board.Rack
rack = Json.Decode.list decodeTile

square : Json.Decode.Decoder Board.Square
square =
  Json.Decode.map3
    Board.Square
    (Json.Decode.field "letterMult" Json.Decode.int)
    (Json.Decode.field "wordMult" Json.Decode.int)
    (Json.Decode.field "squareTile" (Json.Decode.nullable decodeTile))

moveDirection : Json.Decode.Decoder Move.Direction
moveDirection =
  plainVariant
    { name = "moveDirection" }
    [ ( "MoveRight", Move.Right )
    , ( "MoveDown", Move.Down )
    ]

decodeMoveTile : Json.Decode.Decoder Move.Tile
decodeMoveTile =
  variant
    { name = "decodeMoveTile" }
    [ ( "PlaceTile", WithContents (Json.Decode.map Move.PlaceTile decodeTile) )
    , ( "UseBoard", Plain Move.UseBoard )
    ]

move : Json.Decode.Decoder Move
move =
  Json.Decode.map4
    Move
    (Json.Decode.field "startPos" (Json.Decode.index 0 Json.Decode.int))
    (Json.Decode.field "startPos" (Json.Decode.index 1 Json.Decode.int))
    (Json.Decode.field "direction" moveDirection)
    (Json.Decode.field "tiles" (Json.Decode.list decodeMoveTile))

eitherResult : Json.Decode.Decoder err -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result err ok)
eitherResult decodeErr decodeOk =
  let
    ofSides (left, right) =
      case (left, right) of
        (Nothing, Nothing) -> Json.Decode.fail "eitherResult: neither Left nor Right"
        (Just _, Just _) -> Json.Decode.fail "eitherResult: both Left and Right"
        (Just err, Nothing) -> Json.Decode.succeed (Err err)
        (Nothing, Just ok) -> Json.Decode.succeed (Ok ok)
  in
  Json.Decode.map2
    (\x y -> (x, y))
    (Json.Decode.maybe (Json.Decode.field "Left" decodeErr))
    (Json.Decode.maybe (Json.Decode.field "Right" decodeOk))
  |> Json.Decode.andThen ofSides

serverMsg : Json.Decode.Decoder ServerMsg
serverMsg =
  let
    folks =
      Json.Decode.field
        "loggedInOthers"
        (Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string))

    message =
      Json.Decode.map2
        Model.Chat
        (Json.Decode.field "msgSentBy" Json.Decode.string)
        (Json.Decode.field "msgContent" Json.Decode.string)

    posSquare =
      Json.Decode.map3
        (\i j s -> ((i, j), s))
        (Json.Decode.index 0 (Json.Decode.index 0 Json.Decode.int))
        (Json.Decode.index 0 (Json.Decode.index 1 Json.Decode.int))
        (Json.Decode.index 1 square)
    ofPosSquares posSquares =
      case posSquares of
        [] -> Board.empty
        ((ai, aj), asq) :: _ ->
          let
            rowNumbers = List.map (\((i, _), _) -> i) posSquares
            colNumbers = List.map (\((_, j), _) -> j) posSquares
            top = List.foldl min ai rowNumbers
            bottom = List.foldl max ai rowNumbers
            left = List.foldl min aj colNumbers
            right = List.foldl max aj colNumbers
            width = right - left + 1
            height = bottom - top + 1
            posSqDict = Dict.fromList posSquares
            squares =
              Array.initialize height (\row ->
                Array.initialize width (\col ->
                  Dict.get (row + top, col + left) posSqDict
                  |> Maybe.withDefault Board.emptySquare))
          in
          { top = top, left = left, squares = squares }
    board = Json.Decode.map ofPosSquares (Json.Decode.list posSquare)

    moveError =
      variant
        { name = "moveError" }
        [ ( "NotPlaying", Plain Move.NotPlaying )
        , ( "NotYourTurn", Plain Move.NotYourTurn )
        , ( "OffBoard", Plain Move.OffBoard )
        , ( "TilesDoNotMatchBoard", Plain Move.TilesDoNotMatchBoard )
        , ( "NoPlacedTiles", Plain Move.NoPlacedTiles )
        , ( "YouDoNotHave"
          , WithContents
              (Json.Decode.map Move.YouDoNotHave (Json.Decode.list decodeTile))
          )
        , ( "DoesNotConnect", Plain Move.DoesNotConnect )
        , ( "NotAWord"
          , WithContents (Json.Decode.map Move.NotAWord (Json.Decode.list move))
          )
        ]
    moveOk = Json.Decode.succeed ()
    moveResult = eitherResult moveError moveOk
  in
  variant
    { name = "serverMsg" }
    [ ( "Folks", WithFieldsInline (Json.Decode.map Folks folks) )
    , ( "Message", WithFieldsInline (Json.Decode.map Message message) )
    , ( "UpdateBoard", WithContents (Json.Decode.map UpdateBoard board) )
    , ( "UpdateRack", WithContents (Json.Decode.map UpdateRack rack) )
    , ( "MoveResult", WithContents (Json.Decode.map MoveResult moveResult) )
    ]

fromJS : Json.Decode.Decoder FromJS
fromJS =
  variant
    { name = "fromJS" }
    [ ("server-status", WithContents (Json.Decode.map ServerStatus connectionStatus))
    , ("from-server", WithContents (Json.Decode.map FromServer serverMsg))
    ]

toMsg : FromJS -> Msg
toMsg msgFromJS =
  case msgFromJS of
    ServerStatus Connected -> Ok (Msg.PreLogin Msg.Connected)
    ServerStatus Disconnected -> Err Msg.ServerDisconnected
    FromServer (Folks folks) -> Ok (Msg.NewFolks folks)
    FromServer (Message chatMsg) -> Ok (Msg.ReceiveMessage chatMsg)
    FromServer (UpdateBoard board) -> Ok (Msg.UpdateBoard board)
    FromServer (UpdateRack newRack) -> Ok (Msg.UpdateRack newRack)
    FromServer (MoveResult moveResult) -> Ok (Msg.MoveResult moveResult)

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map toMsg fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
