{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import System.Environment (getArgs)

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
newServerState = ServerState <$> newMVar newGame <*> newChan

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

sendBroadcast :: ServerState -> ToClient -> IO ()
sendBroadcast ServerState{ broadcast } msg = do
  writeChan broadcast msg

loggedIn :: ServerState -> WS.Connection -> Text -> IO ()
loggedIn state@ServerState{ gameStore } conn username = do
  print ("logged in", username)
  modifyMVar_ gameStore $ \game -> do
    let newFolks = Set.insert username (folks game)
    sendBroadcast state Folks{ loggedInOthers = newFolks }
    return game{ folks = newFolks }
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead state conn username)
    (writeThread state conn username)
    `onException` do
      modifyMVar_ gameStore $ \game -> do
        print ("disconnected", username)
        let newFolks = Set.delete username (folks game)
        sendBroadcast state Folks{ loggedInOthers = newFolks }
        return game{ folks = newFolks }
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

playerRead :: ServerState -> WS.Connection -> Text -> IO Void
playerRead serverState conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print ("unexpected second login", username, msg)
    Just Chat{ msgToSend } ->
      sendBroadcast serverState Message{ msgSentBy = username, msgContent = msgToSend }
    Just (MakeMove move) ->
      modifyMVar_ (gameStore serverState) $ \game ->
        case applyMove move (board game) of
          Right newBoard -> do
            sendToClient conn (MoveResult (Right ()))
            sendBroadcast serverState (UpdateBoard newBoard)
            return game{ board = newBoard }
          Left moveError -> do
            sendToClient conn (MoveResult (Left moveError))
            return game

writeThread :: ServerState -> WS.Connection -> Text -> IO Void
writeThread ServerState{ gameStore, broadcast } conn _username = do
  withMVar gameStore $ \game -> do
    sendToClient conn Folks{ loggedInOthers = folks game }
    sendToClient conn (UpdateBoard (board game))
    sendToClient conn (UpdateRack (Rack
        [ Tile{ tileChar = 'A', tileScore = 1 }
        , Tile{ tileChar = 'B', tileScore = 3 }
        , Tile{ tileChar = 'C', tileScore = 3 }
        , Tile{ tileChar = 'D', tileScore = 2 }
        , Tile{ tileChar = 'E', tileScore = 1 }
        , Tile{ tileChar = 'F', tileScore = 4 }
        ]
      ))
  forever $ do
    sendToClient conn =<< readChan broadcast
