{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Protocol

data ServerState
  = ServerState{ folksStore :: MVar (Set Text), broadcast :: Chan ToClient }

newServerState :: IO ServerState
newServerState = ServerState <$> newMVar Set.empty <*> newChan

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
loggedIn state@ServerState{ folksStore } conn username = do
  print ("logged in", username)
  modifyMVar_ folksStore $ \folks -> do
    let newFolks = Set.insert username folks
    sendBroadcast state Folks{ loggedInOthers = newFolks }
    return newFolks
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (memberRead state conn username)
    (writeThread state conn username)
    `onException` do
      modifyMVar_ folksStore $ \folks -> do
        print ("disconnected", username)
        let newFolks = Set.delete username folks
        sendBroadcast state Folks{ loggedInOthers = newFolks }
        return newFolks
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

memberRead :: ServerState -> WS.Connection -> Text -> IO Void
memberRead serverState conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print ("unexpected second login", username, msg)
    Just Chat{ msgToSend } ->
      sendBroadcast serverState Message{ msgSentBy = username, msgContent = msgToSend }

writeThread :: ServerState -> WS.Connection -> Text -> IO Void
writeThread ServerState{ folksStore, broadcast } conn _username = do
  withMVar folksStore $ \folks -> sendToClient conn Folks{ loggedInOthers = folks }
  forever $ do
    sendToClient conn =<< readChan broadcast
