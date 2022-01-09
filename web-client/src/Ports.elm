port module Ports exposing (..)

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
  in
  variant
    { name = "serverMsg" }
    [ ( "Folks", WithFieldsInline (Json.Decode.map Folks folks) )
    , ( "Message", WithFieldsInline (Json.Decode.map Message message) )
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

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map toMsg fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
