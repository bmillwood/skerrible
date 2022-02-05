port module Ports exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Set exposing (Set)

import Model
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

contentsWithTag : String -> Json.Encode.Value -> Json.Encode.Value
contentsWithTag tag contents =
  Json.Encode.object
    [ ( "tag", Json.Encode.string tag )
    , ( "contents", contents )
    ]

login : { username : String } -> Cmd msg
login { username } =
  Json.Encode.object
    [ ( "tag", Json.Encode.string "LoginRequest" )
    , ( "loginRequestName", Json.Encode.string username )
    ]
  |> send

chat : String -> Cmd msg
chat message =
  Json.Encode.object
    [ ( "tag", Json.Encode.string "Chat" )
    , ( "msgToSend", Json.Encode.string message )
    ]
  |> send

encodeDirection : Model.MoveDirection -> Json.Encode.Value
encodeDirection d =
  Json.Encode.string (
    case d of
      Model.MoveRight -> "MoveRight"
      Model.MoveDown -> "MoveDown"
  )

encodeTile : Model.Tile -> Json.Encode.Value
encodeTile { char, score } =
  Json.Encode.object
    [ ( "tileChar", Json.Encode.string (String.fromChar char) )
    , ( "tileScore", Json.Encode.int score )
    ]

sendMove : Model.Move -> Cmd msg
sendMove { startPos, direction, tiles } =
  let
    (si, sj) = startPos
  in
  Json.Encode.object
    [ ( "startPos", Json.Encode.list Json.Encode.int [si, sj] )
    , ( "direction", encodeDirection direction )
    , ( "tiles", Json.Encode.list encodeTile tiles )
    ]
  |> contentsWithTag "MakeMove"
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
  | UpdateBoard Model.Board
  | UpdateRack Model.Rack

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

decodeChar : Json.Decode.Decoder Char
decodeChar =
  Json.Decode.string
  |> Json.Decode.andThen (\s ->
        case String.uncons s of
          Nothing -> Json.Decode.fail "JSON char: empty string"
          Just (c, r) ->
            if not (String.isEmpty r)
            then Json.Decode.fail <|
              "JSON char: String " ++ Debug.toString s ++ " is not length 1"
            else Json.Decode.succeed c
      )

tile : Json.Decode.Decoder Model.Tile
tile =
  Json.Decode.map2
    Model.Tile
    (Json.Decode.field "tileChar" decodeChar)
    (Json.Decode.field "tileScore" Json.Decode.int)

square : Json.Decode.Decoder Model.Square
square =
  Json.Decode.map3
    Model.Square
    (Json.Decode.field "letterMult" Json.Decode.int)
    (Json.Decode.field "wordMult" Json.Decode.int)
    (Json.Decode.field "squareTile" (Json.Decode.nullable tile))

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
        [] -> Model.emptyBoard
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
                  |> Maybe.withDefault Model.emptySquare))
          in
          { topLeft = (top, left), squares = squares }

    board = Json.Decode.map ofPosSquares (Json.Decode.list posSquare)

    rack = Json.Decode.list tile
  in
  variant
    { name = "serverMsg" }
    [ ( "Folks", WithFieldsInline (Json.Decode.map Folks folks) )
    , ( "Message", WithFieldsInline (Json.Decode.map Message message) )
    , ( "UpdateBoard", WithContents (Json.Decode.map UpdateBoard board) )
    , ( "UpdateRack", WithContents (Json.Decode.map UpdateRack rack) )
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
    FromServer (UpdateRack rack) -> Ok (Msg.UpdateRack rack)

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map toMsg fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
