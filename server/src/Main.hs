{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Function (fix)
import Data.Map (Map)
import Data.String (fromString)
import Data.Void
import System.Environment (getArgs, lookupEnv)
import qualified System.Random as Random

import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Game
import Protocol

data RoomState =
  RoomState
    { gameStore :: MVar GameState
    , roomCode :: RoomCode
    , clients :: MVar (Map Username (Chan ToClient))
    }

newRoomState :: RoomCode -> RoomSettings -> IO RoomState
newRoomState roomCode roomSettings = do
  gameStore <- newMVar =<< createGame roomSettings <$> Random.newStdGen
  clients <- newMVar Map.empty
  return RoomState{ gameStore, roomCode, clients }

data ServerState = ServerState (MVar (Map RoomCode RoomState))

newServerState :: IO ServerState
newServerState = ServerState <$> newMVar Map.empty

broadcast :: RoomState -> ToClient -> IO ()
broadcast RoomState{ clients, roomCode } msg = do
  print (roomCode, "broadcast", msg)
  withMVar clients $ \clientMap ->
    mapM_ (\chan -> writeChan chan msg) clientMap

warpSettings :: IO Warp.Settings
warpSettings = do
  host <- fromString . maybe "localhost" id <$> lookupEnv "HOST"
  port <- maybe portNumber read <$> lookupEnv "PORT"
  return
    $ Warp.setHost host
    $ Warp.setPort port
    $ Warp.defaultSettings

main :: IO ()
main = do
  [staticPath] <- getArgs
  state <- newServerState
  settings <- warpSettings
  Warp.runSettings settings (waiApp state staticPath)

waiApp :: ServerState -> FilePath -> Wai.Application
waiApp state staticPath =
  WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (staticApp staticPath)

logStaticLookups :: WaiStatic.StaticSettings -> WaiStatic.StaticSettings
logStaticLookups settings =
  settings
    { WaiStatic.ssLookupFile = \pieces ->
        print ("static", pieces) >> WaiStatic.ssLookupFile settings pieces
    }

staticApp :: FilePath -> Wai.Application
staticApp staticPath =
  WaiStatic.staticApp settings
  where
    settings =
      logStaticLookups
      $ WaiStatic.defaultFileServerSettings staticPath

wsApp :: ServerState -> WS.ServerApp
wsApp state = handleConnection state <=< WS.acceptRequest

sendToConn :: WS.Connection -> ToClient -> IO ()
sendToConn conn msg = do
  WS.sendTextData conn (Aeson.encode msg)

sendToClient :: RoomState -> Username -> ToClient -> IO ()
sendToClient RoomState{ clients, roomCode } username msg =
  withMVar clients $ \clientsMap ->
    case Map.lookup username clientsMap of
      Nothing -> print (roomCode, "sendToClient: no client", username)
      Just chan -> writeChan chan msg

readFromClient :: WS.Connection -> IO (Maybe FromClient)
readFromClient conn = do
  json <- WS.receiveData conn
  case Aeson.decode json of
    Nothing -> do
      putStrLn ("couldn't decode: " ++ show json)
      return Nothing
    Just msg -> return (Just msg)

handleConnection :: ServerState -> WS.Connection -> IO ()
handleConnection (ServerState roomsVar) conn = do
  print "handleConnection"
  fix $ \loop -> do
    msg <- readFromClient conn
    case msg of
      Nothing -> return ()
      Just LoginRequest{ loginRequestName, roomSpec } ->
        case validUsername loginRequestName of
          Just usernameError -> do
            print ("login failed", usernameError, loginRequestName, roomSpec)
            sendToConn conn (TechnicalError usernameError)
            loop
          Nothing ->
            WS.withPingThread conn 30 (print ("ping", loginRequestName)) $ do
              print ("logged in", loginRequestName, roomSpec)
              join $ modifyMVar roomsVar $ \rooms -> do
                case roomSpec of
                  JoinRoom code ->
                    case Map.lookup code rooms of
                      Nothing -> do
                        sendToConn conn RoomDoesNotExist
                        return (rooms, loop)
                      Just room ->
                        return (rooms, joinedRoom room conn loginRequestName)
                  MakeNewRoom roomSettings -> do
                    code <- unusedRoomCode 4
                    room <- newRoomState code roomSettings
                    return
                      ( Map.insert code room rooms
                      , joinedRoom room conn loginRequestName
                      )
                   where
                    unusedRoomCode len = do
                      code <- RoomCode . fromString
                        <$> replicateM len (Random.randomRIO ('A', 'Z'))
                      if Map.member code rooms
                        then unusedRoomCode (len + 1)
                        else return code

      Just other -> do
        sendToConn conn (TechnicalError ProtocolError)
        print ("unexpected message, dropping connection", other)

updatePlayers :: (GameState -> GameState) -> RoomState -> IO ()
updatePlayers up state@RoomState{ gameStore } = do
  modifyMVar_ gameStore $ \game -> do
    let updatedGame = up game
    broadcast state (Scores (scores updatedGame))
    return updatedGame

newtype IOAnd c a = IOAnd { runIOAnd :: IO (c, a) } deriving (Functor)

addClient :: Maybe (Chan ToClient) -> IOAnd (Chan ToClient) (Maybe (Chan ToClient))
addClient Nothing = IOAnd $ do
  c <- newChan
  return (c, Just c)
addClient (Just c) = IOAnd $ do
  newC <- dupChan c
  return (newC, Just c)

joinedRoom :: RoomState -> WS.Connection -> Username -> IO ()
joinedRoom state@RoomState{ clients, roomCode } conn username = do
  sendToConn conn (UpdateRoomCode roomCode)
  toClientChan <- modifyMVar clients $ \clientMap -> do
    (chan, newClients) <- runIOAnd (Map.alterF addClient username clientMap)
    return (newClients, chan)
  updatePlayers (addPlayerIfAbsent username) state
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead state conn username)
    (writeThread state toClientChan conn username)
    `onException` do
      print (roomCode, "disconnected", username)
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

playerRead :: RoomState -> WS.Connection -> Username -> IO Void
playerRead roomState@RoomState{ roomCode } conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print (roomCode, "unexpected second login", username, msg)
    Just Chat{ msgToSend } ->
      case validChat msgToSend of
        Just chatError -> do
          print (roomCode, "bad chat", chatError, msgToSend)
          sendToConn conn (TechnicalError chatError)
        Nothing ->
          broadcast roomState
            ChatMessage{ chatSentBy = username, chatContent = msgToSend }
    Just (MakeMove move) ->
      modifyMVar_ (gameStore roomState) $ \game ->
        case applyMove username move game of
          Right (nextGame@GameState{ board, players }, moveReport) -> do
            sendToClient roomState username (MoveResult (Right ()))
            case Map.lookup username players of
              Nothing -> print (roomCode, "sendRack: player missing", username)
              Just PlayerState{ rack } ->
                sendToClient roomState username (UpdateRack rack)
            mapM_ (broadcast roomState)
              [ PlayerMoved moveReport
              , Scores (scores nextGame)
              , UpdateBoard board
              ]
            return nextGame
          Left moveError -> do
            sendToClient roomState username (MoveResult (Left moveError))
            return game

writeThread :: RoomState -> Chan ToClient -> WS.Connection -> Username -> IO Void
writeThread RoomState{ gameStore, roomCode } toClientChan conn username = do
  withMVar gameStore $ \GameState{ board, players } -> do
    writeChan toClientChan (UpdateBoard board)
    writeChan toClientChan (UpdateTileData tileData)
    case Map.lookup username players of
      Nothing -> print (roomCode, "sendRack: player missing", username)
      Just PlayerState{ rack } -> writeChan toClientChan (UpdateRack rack)
  forever $ do
    sendToConn conn =<< readChan toClientChan
