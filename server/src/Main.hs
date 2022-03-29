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

data ServerState =
  ServerState
    { gameStore :: MVar GameState
    , clients :: MVar (Map Username (Chan ToClient))
    }

newServerState :: IO ServerState
newServerState = do
  gameStore <- newMVar =<< (createGame <$> Random.newStdGen)
  clients <- newMVar Map.empty
  return ServerState{ gameStore, clients }

broadcast :: ServerState -> ToClient -> IO ()
broadcast ServerState{ clients } msg = do
  print ("broadcast", msg)
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

sendToClient :: ServerState -> Username -> ToClient -> IO ()
sendToClient ServerState{ clients } username msg =
  withMVar clients $ \clientsMap ->
    case Map.lookup username clientsMap of
      Nothing -> print ("sendToClient: no client", username)
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
handleConnection state conn = do
  print "handleConnection"
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{ loginRequestName } ->
      WS.withPingThread conn 30 (print ("ping", loginRequestName)) $ do
        loggedIn state conn loginRequestName
    Just other -> print ("unexpected message", other)

updatePlayers :: (GameState -> GameState) -> ServerState -> IO ()
updatePlayers up state@ServerState{ gameStore } = do
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

loggedIn :: ServerState -> WS.Connection -> Username -> IO ()
loggedIn state@ServerState{ clients } conn username = do
  print ("logged in", username)
  toClientChan <- modifyMVar clients $ \clientMap -> do
    (chan, newClients) <- runIOAnd (Map.alterF addClient username clientMap)
    return (newClients, chan)
  -- the client depends on the Scores update from updatePlayers being the first
  -- game message it receives, so uh, keep that in mind
  updatePlayers (addPlayerIfAbsent username) state
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead state conn username)
    (writeThread state toClientChan conn username)
    `onException` do
      print ("disconnected", username)
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

playerRead :: ServerState -> WS.Connection -> Username -> IO Void
playerRead serverState conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print ("unexpected second login", username, msg)
    Just Chat{ msgToSend } ->
      broadcast serverState
        ChatMessage{ chatSentBy = username, chatContent = msgToSend }
    Just (MakeMove move) ->
      modifyMVar_ (gameStore serverState) $ \game ->
        case applyMove username move game of
          Right (nextGame@GameState{ board, players }, moveReport) -> do
            sendToClient serverState username (MoveResult (Right ()))
            broadcast serverState (PlayerMoved moveReport)
            case Map.lookup username players of
              Nothing -> print ("sendRack: player missing", username)
              Just PlayerState{ rack } ->
                sendToClient serverState username (UpdateRack rack)
            broadcast serverState (Scores (scores nextGame))
            broadcast serverState (UpdateBoard board)
            return nextGame
          Left moveError -> do
            sendToClient serverState username (MoveResult (Left moveError))
            return game

writeThread :: ServerState -> Chan ToClient -> WS.Connection -> Username -> IO Void
writeThread ServerState{ gameStore } toClientChan conn username = do
  withMVar gameStore $ \GameState{ board, players } -> do
    writeChan toClientChan (UpdateBoard board)
    writeChan toClientChan (UpdateTileData tileData)
    case Map.lookup username players of
      Nothing -> print ("sendRack: player missing", username)
      Just PlayerState{ rack } -> writeChan toClientChan (UpdateRack rack)
  forever $ do
    sendToConn conn =<< readChan toClientChan
