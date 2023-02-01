port module Ports exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
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

login : Model.LoginForm -> Cmd msg
login { username, roomAction, roomCode, roomSettings } =
  let
    roomSpec =
      case roomAction of
        Model.JoinRoom ->
          withTag "JoinRoom" [("contents", Json.Encode.string roomCode)]
        Model.MakeNewRoom ->
          let
            { noBoardMultipliers, turnEnforcement } = roomSettings
            encodedTurnEnforcement =
              Json.Encode.string
              <| case turnEnforcement of
                Model.NoEnforcement -> "NoEnforcement"
                Model.LetPlayersChoose -> "LetPlayersChoose"
          in
          withTag "MakeNewRoom"
            [ ( "contents"
              , Json.Encode.object
                  [ ( "noBoardMultipliers", Json.Encode.bool noBoardMultipliers )
                  , ( "turnEnforcement", encodedTurnEnforcement )
                  ]
              )
            ]
  in
  withTag "LoginRequest"
  [ ( "loginRequestName", Json.Encode.string username )
  , ( "roomSpec", roomSpec )
  ] |> send

joinGame : Cmd msg
joinGame = send (withTag "JoinGame" [])

chat : String -> Cmd msg
chat message =
  withTag "Chat" [("msgToSend", Json.Encode.string message)]
  |> send

sendPass : Cmd msg
sendPass = send (withTag "Pass" [])

sendUndo : Cmd msg
sendUndo = send (withTag "Undo" [])

encodeDirection : Move.Direction -> Json.Encode.Value
encodeDirection d =
  Json.Encode.string (
    case d of
      Move.Right -> "MoveRight"
      Move.Down -> "MoveDown"
  )

encodeTile : Board.Tile -> Json.Encode.Value
encodeTile tile =
  case tile of
    Board.Letter c ->
      withTag "Letter"
        [("contents", Json.Encode.string (String.fromChar c))]
    Board.Blank ->
      withTag "Blank" []

sendExchange : List Board.Tile -> Cmd msg
sendExchange tiles =
  case tiles of
    [] -> Cmd.none
    _ -> send (withTag "Exchange" [("contents", Json.Encode.list encodeTile tiles)])

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

-- if any variant has data, all of them are represented as an object with a
-- tag field
variantWithFields
  : { name : String } -> List (String, VariantSpec a) -> Json.Decode.Decoder a
variantWithFields { name } values =
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

connectionStatus : Json.Decode.Decoder ConnectionStatus
connectionStatus =
  plainVariant
    { name = "connectionStatus" }
    [ ("connected", Connected)
    , ("disconnected", Disconnected)
    ]

decodeTile : Json.Decode.Decoder Board.Tile
decodeTile =
  variantWithFields
    { name = "decodeTile" }
    [ ("Blank", Plain Board.Blank)
    , ("Letter", WithContents (Json.Decode.map Board.Letter Key.decodeChar))
    ]

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
  variantWithFields
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

listOfPairs
  :  Json.Decode.Decoder a
  -> Json.Decode.Decoder b
  -> Json.Decode.Decoder (List (a, b))
listOfPairs decodeA decodeB =
  Json.Decode.list (
    Json.Decode.map2
      (\x y -> (x, y))
      (Json.Decode.index 0 decodeA)
      (Json.Decode.index 1 decodeB)
  )

serverMsg : Json.Decode.Decoder Msg.OneMsg
serverMsg =
  let
    tooLong =
      Json.Decode.map2
        (\used limit ->
          "length " ++ String.fromInt used
          ++ " exceeds limit " ++ String.fromInt limit)
        (Json.Decode.field "lengthUsed" Json.Decode.int)
        (Json.Decode.field "lengthLimit" Json.Decode.int)
    techError =
      variantWithFields
        { name = "TechErrorMsg" }
        [ ( "ProtocolError", Plain (Msg.clientError "Unspecified protocol error :(") )
        , ( "MustNotBeEmpty", Plain (Msg.clientError "Must not be empty") )
        , ( "TooLong", WithFieldsInline (Json.Decode.map Msg.clientError tooLong) )
        ]

    updateRoomCode = Json.Decode.map (Msg.PreLogin << Msg.UpdateRoomCode) Json.Decode.string

    people =
      Json.Decode.list Json.Decode.string
      |> Json.Decode.map (Msg.InRoom << Msg.UpdatePeople << Set.fromList)

    scores =
      listOfPairs Json.Decode.string Json.Decode.int
      |> Json.Decode.map (Msg.InRoom << Msg.UpdateScores << Dict.fromList)

    message =
      Json.Decode.map2
        Model.Chat
        (Json.Decode.field "chatSentBy" Json.Decode.string)
        (Json.Decode.field "chatContent" Json.Decode.string)
      |> Json.Decode.map (Msg.InRoom << Msg.ReceiveChatMessage)

    playedWord =
      Json.Decode.map2
        (\words score -> Model.PlayedWord { words = words, score = score })
        (Json.Decode.field "moveWords" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "moveScore" Json.Decode.int)
    exchanged = Json.Decode.map Model.Exchanged Json.Decode.int
    moveReport =
      variantWithFields
        { name = "playerMoved" }
        [ ( "PlayedWord", WithFieldsInline playedWord )
        , ( "Exchanged", WithContents exchanged )
        , ( "Passed", Plain Model.Passed )
        , ( "Undone", Plain Model.Undone )
        ]
    playerMoved =
      Json.Decode.map2
        (\player report ->
          Msg.InRoom (Msg.ReceiveMove { player = player, moveReport = report })
        )
        (Json.Decode.field "movePlayer" Json.Decode.string)
        (Json.Decode.field "moveReport" moveReport)

    tileData =
      Json.Decode.map2
        Board.TileData
        (Json.Decode.field "tileScore" Json.Decode.int)
        (Json.Decode.field "tileCount" Json.Decode.int)
    updateTileData =
      listOfPairs decodeTile tileData
      |> Json.Decode.map (Msg.InRoom << Msg.UpdateTileData << DictTile.fromList)

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
    updateBoard =
      Json.Decode.list posSquare
      |> Json.Decode.map (Msg.InRoom << Msg.UpdateBoard << ofPosSquares)

    updateRack =
      Json.Decode.list decodeTile
      |> Json.Decode.map (Msg.InRoom << Msg.UpdateRack)

    moveError =
      variantWithFields
        { name = "moveError" }
        [ ( "YouAreNotPlaying", Plain Move.YouAreNotPlaying )
        , ( "GameIsOver", Plain Move.GameIsOver )
        , ( "NotYourTurn", Plain Move.NotYourTurn )
        , ( "OffBoard", Plain Move.OffBoard )
        , ( "TilesDoNotMatchBoard", Plain Move.TilesDoNotMatchBoard )
        , ( "NoPlacedTiles", Plain Move.NoPlacedTiles )
        , ( "YouDoNotHave"
          , WithContents
              (Json.Decode.map Move.YouDoNotHave (Json.Decode.list decodeTile))
          )
        , ( "FirstMoveNotInCentre", Plain Move.FirstMoveNotInCentre )
        , ( "NoMultiletterWordsMade", Plain Move.NoMultiletterWordsMade )
        , ( "DoesNotConnect", Plain Move.DoesNotConnect )
        , ( "NotAWord"
          , WithContents (Json.Decode.map Move.NotAWord (Json.Decode.list move))
          )
        , ( "NotEnoughTilesToExchange", Plain Move.NotEnoughTilesToExchange )
        ]
    moveOk = Json.Decode.succeed ()
    moveResult =
      eitherResult moveError moveOk
      |> Json.Decode.map (Msg.InRoom << Msg.MoveResult)
  in
  variantWithFields
    { name = "serverMsg" }
    [ ( "TechnicalError" , WithContents techError )
    , ( "RoomDoesNotExist", Plain (Msg.PreLogin Msg.NoSuchRoom) )
    , ( "UpdateRoomCode", WithContents updateRoomCode )
    , ( "People", WithContents people )
    , ( "Scores", WithContents scores )
    , ( "ChatMessage", WithFieldsInline message )
    , ( "PlayerMoved", WithFieldsInline playerMoved )
    , ( "UpdateTileData", WithContents updateTileData )
    , ( "UpdateBoard", WithContents updateBoard )
    , ( "UpdateRack", WithContents updateRack )
    , ( "MoveResult", WithContents moveResult )
    , ( "GameOver", Plain (Msg.InRoom Msg.GameOver) )
    ]

fromJS : Json.Decode.Decoder Msg.OneMsg
fromJS =
  let
    ofConnStatus status =
      case status of
        Connected -> Msg.PreLogin Msg.Connected
        Disconnected -> Msg.serverDisconnected
  in
  variantWithFields
    { name = "fromJS" }
    [ ("server-status", WithContents (Json.Decode.map ofConnStatus connectionStatus))
    , ("from-server", WithContents serverMsg)
    ]

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue fromJS value of
      Ok msg -> [msg]
      Err error -> [Msg.driverProtocolError (Json.Decode.errorToString error)]
