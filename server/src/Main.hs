{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Lens (ifor_)
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

data RoomState
  = RoomState
    { gameStore :: MVar Game
    , roomCode :: RoomCode
    , clients :: MVar (Map Username (Integer, Chan ToClient))
    , setStale :: Bool -> IO ()
    }

newRoomState :: RoomCode -> RoomSettings -> (Bool -> IO ()) -> IO RoomState
newRoomState roomCode roomSettings setStale = do
  gameStore <- newMVar =<< createGame roomSettings <$> Random.newStdGen
  clients <- newMVar Map.empty
  return RoomState{ gameStore, roomCode, clients, setStale }

data ServerState = ServerState (MVar (Map RoomCode RoomState))

newServerState :: IO ServerState
newServerState = ServerState <$> newMVar Map.empty

staleSetter :: RoomCode -> ServerState -> IO (Bool -> IO ())
staleSetter code (ServerState roomMapVar) = do
  collectorStore <- newMVar Nothing
  return $ \shouldCollect -> do
    modifyMVar_ collectorStore $ \collector -> do
      case collector of
        Nothing -> return ()
        Just c -> do
          print (code, "cancelling cleanup")
          cancel c
      if shouldCollect
        then do
          print (code, "scheduling cleanup")
          newCollector <- async $ do
            threadDelay (3600 * 1000000)
            print (code, "removing")
            modifyMVar_ roomMapVar $ \roomMap ->
              return (Map.delete code roomMap)
          return (Just newCollector)
        else return Nothing

broadcast :: RoomState -> ToClient -> IO ()
broadcast RoomState{ clients, roomCode } msg = do
  print (roomCode, "broadcast", msg)
  withMVar clients $ \clientMap ->
    mapM_ (\(_, chan) -> writeChan chan msg) clientMap

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
      Just (_, chan) -> writeChan chan msg

readFromClient :: WS.Connection -> IO (Maybe FromClient)
readFromClient conn = do
  json <- WS.receiveData conn
  case Aeson.decode json of
    Nothing -> do
      putStrLn ("couldn't decode: " ++ show json)
      return Nothing
    Just msg -> return (Just msg)

handleConnection :: ServerState -> WS.Connection -> IO ()
handleConnection state@(ServerState roomsVar) conn = do
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
                    setStale <- staleSetter code state
                    room <- newRoomState code roomSettings setStale
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

data Undoable
  = CanUndo
  | CannotUndo

modifyGame :: Undoable -> RoomState -> (GameState -> IO (GameState, a)) -> IO a
modifyGame undoable RoomState{ gameStore } change = do
  modifyMVar gameStore $ \game -> do
    (nextState, result) <- change (latestState game)
    let
      apply = case undoable of
        CanUndo -> addState
        CannotUndo -> setLatestState
    return (apply nextState game, result)

applyUndo :: RoomState -> Username -> IO ()
applyUndo roomState@RoomState{ gameStore } username = do
  modifyMVar_ gameStore $ \game ->
    case undo game of
      Nothing -> do
        return game
      Just undone -> do
        let latest@GameState{ board = uBoard, players = uPlayers } = latestState undone
        ifor_ uPlayers $ \uName PlayerState{ rack } ->
          sendToClient roomState uName (UpdateRack rack)
        mapM_ (broadcast roomState)
          [ Scores (scores latest)
          , UpdateBoard uBoard
          , PlayerMoved { movePlayer = username, moveReport = Undone }
          ]
        return undone

newtype IOAnd c a = IOAnd { runIOAnd :: IO (c, a) } deriving (Functor)

addClient :: Maybe (Integer, Chan ToClient) -> IOAnd (Chan ToClient) (Maybe (Integer, Chan ToClient))
addClient Nothing = IOAnd $ do
  c <- newChan
  return (c, Just (1, c))
addClient (Just (n, c)) = IOAnd $ do
  newC <- dupChan c
  return (newC, Just (n + 1, c))

removeClient :: Maybe (Integer, Chan ToClient) -> Maybe (Integer, Chan ToClient)
removeClient Nothing = Nothing
removeClient (Just (n, c))
  | n <= 1 = Nothing
  | otherwise = Just (n - 1, c)

joinedRoom :: RoomState -> WS.Connection -> Username -> IO ()
joinedRoom state@RoomState{ clients, roomCode, setStale } conn username = do
  setStale False
  sendToConn conn (UpdateRoomCode roomCode)
  toClientChan <- modifyMVar clients $ \clientMap -> do
    (chan, newClients) <- runIOAnd (Map.alterF addClient username clientMap)
    return (newClients, chan)
  modifyGame CannotUndo state $ \gameState -> do
    let withPlayer = addPlayerIfAbsent username gameState
    broadcast state (Scores (scores withPlayer))
    return (withPlayer, ())
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead state conn username)
    (writeThread state toClientChan conn username)
    `onException` do
      modifyMVar_ clients $ \clientMap -> do
        print (roomCode, "disconnected", username)
        let newMap = Map.alter removeClient username clientMap
        when (Map.null newMap) (setStale True)
        return newMap
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
    Just (MakeMove move) -> doMove (applyMove username move)
    Just (Exchange tiles) -> doMove (applyExchange username tiles)
    Just Pass -> doMove (applyPass username)
    Just Undo -> applyUndo roomState username
  where
    doMove applyToGame =
      modifyGame CanUndo roomState $ \game ->
        case applyToGame game of
          Right (moveReport, nextGame@GameState{ board, players }) -> do
            sendToClient roomState username (MoveResult (Right ()))
            case Map.lookup username players of
              Nothing -> print (roomCode, "sendRack: player missing", username)
              Just PlayerState{ rack } ->
                sendToClient roomState username (UpdateRack rack)
            mapM_ (broadcast roomState)
              [ PlayerMoved { movePlayer = username, moveReport }
              , Scores (scores nextGame)
              , UpdateBoard board
              ]
            case nextGame of
              GameState{ gameOver = True } ->
                mapM_ (broadcast roomState)
                  [ Scores (scores nextGame)
                  , GameOver
                  ]
              _ -> return ()
            return (nextGame, ())
          Left moveError -> do
            sendToClient roomState username (MoveResult (Left moveError))
            return (game, ())

writeThread :: RoomState -> Chan ToClient -> WS.Connection -> Username -> IO Void
writeThread RoomState{ gameStore, roomCode } toClientChan conn username = do
  withMVar gameStore $ \game -> do
    let GameState{ board, players } = latestState game
    mapM_ (writeChan toClientChan)
      [ UpdateBoard board
      , UpdateTileData tileData
      ]
    case Map.lookup username players of
      Nothing -> print (roomCode, "sendRack: player missing", username)
      Just PlayerState{ rack } -> writeChan toClientChan (UpdateRack rack)
  forever $ do
    sendToConn conn =<< readChan toClientChan
