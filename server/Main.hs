{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void
import System.Environment (getArgs)
import qualified System.Random as Random

import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Game
import Protocol

data ServerState
  = ServerState{ gameStore :: MVar GameState, broadcast :: Chan ToClient }

newServerState :: IO ServerState
newServerState = do
  gameStore <- newMVar =<< (createGame <$> Random.newStdGen)
  broadcast <- newChan
  return ServerState{ gameStore, broadcast }

main :: IO ()
main = do
  [staticPath] <- getArgs
  state@ServerState{ broadcast } <- newServerState
  _ <- forkIO . forever $ do
    msg <- readChan broadcast
    print ("broadcast", msg)
  Warp.runEnv portNumber (waiApp state staticPath)

waiApp :: ServerState -> FilePath -> Wai.Application
waiApp state staticPath =
  WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (staticApp staticPath)

staticApp :: FilePath -> Wai.Application
staticApp staticPath =
  WaiStatic.staticApp (WaiStatic.defaultFileServerSettings staticPath)

wsApp :: ServerState -> WS.ServerApp
wsApp state = handleConnection state <=< WS.acceptRequest

sendToClient :: WS.Connection -> ToClient -> IO ()
sendToClient conn msg = do
  WS.sendTextData conn (Aeson.encode msg)

readFromClient :: WS.Connection -> IO (Maybe FromClient)
readFromClient conn = do
  json <- WS.receiveData conn
  case Aeson.decode json of
    Nothing -> do
      putStrLn ("couldn't decode: " ++ show json)
      return Nothing
    Just msg -> return (Just msg)

handleConnection :: ServerState -> WS.Connection -> IO ()
handleConnection state@ServerState{ broadcast } conn = do
  print "handleConnection"
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{ loginRequestName } ->
      WS.withPingThread conn 30 (print ("ping", loginRequestName)) $ do
        clientBroadcast <- dupChan broadcast
        loggedIn state{ broadcast = clientBroadcast } conn loginRequestName
    Just other -> putStrLn ("unexpected message: " ++ show other)

folksMsg :: GameState -> ToClient
folksMsg game = Folks{ loggedInOthers = Map.keysSet (players game) }

updatePlayers :: (GameState -> GameState) -> ServerState -> IO ()
updatePlayers up ServerState{ broadcast, gameStore } = do
  modifyMVar_ gameStore $ \game -> do
    let updatedGame = up game
    writeChan broadcast (folksMsg updatedGame)
    return updatedGame

loggedIn :: ServerState -> WS.Connection -> Text -> IO ()
loggedIn state conn username = do
  print ("logged in", username)
  updatePlayers (addPlayer username) state
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead state conn username)
    (writeThread state conn username)
    `onException` do
      print ("disconnected", username)
      updatePlayers (removePlayer username) state
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

sendRack :: WS.Connection -> Text -> GameState -> IO ()
sendRack conn username GameState{ players } =
  case Map.lookup username players of
    Nothing -> print ("sendRack: player missing", username)
    Just PlayerState{ rack } -> sendToClient conn (UpdateRack rack)

playerRead :: ServerState -> WS.Connection -> Text -> IO Void
playerRead serverState conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print ("unexpected second login", username, msg)
    Just Chat{ msgToSend } ->
      writeChan (broadcast serverState)
        Message{ msgSentBy = username, msgContent = msgToSend }
    Just (MakeMove move) ->
      modifyMVar_ (gameStore serverState) $ \game ->
        case applyMove username move game of
          Right nextGame@GameState{ board = newBoard } -> do
            sendToClient conn (MoveResult (Right ()))
            sendRack conn username nextGame
            writeChan (broadcast serverState) (UpdateBoard newBoard)
            return nextGame
          Left moveError -> do
            sendToClient conn (MoveResult (Left moveError))
            return game

writeThread :: ServerState -> WS.Connection -> Text -> IO Void
writeThread ServerState{ gameStore, broadcast } conn username = do
  withMVar gameStore $ \game -> do
    sendToClient conn (folksMsg game)
    sendToClient conn (UpdateBoard (board game))
    sendToClient conn (UpdateTileData tileData)
    sendRack conn username game
  forever $ do
    sendToClient conn =<< readChan broadcast
