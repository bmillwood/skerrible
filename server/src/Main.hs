{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Lens (ifor_)
import Control.Monad
import Control.Monad.Trans.Writer
import qualified Data.Map as Map
import Data.Function (fix)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Map (Map)
import Data.Monoid (Endo(Endo))
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

data Client
  = Client
    { connectionCount :: Integer
    , ear :: Chan ToClient
    }

data RoomState
  = RoomState
    { game :: Game
    , clients :: Map Username Client
    }

data Room
  = Room
    { roomCode :: RoomCode
    , roomState :: MVar RoomState
    , setStale :: Bool -> IO ()
    }

newRoom :: RoomCode -> RoomSettings -> (Bool -> IO ()) -> IO Room
newRoom roomCode roomSettings setStale = do
  game <- createGame roomSettings <$> Random.newStdGen
  roomState <- newMVar RoomState{ game, clients = Map.empty }
  return Room{ roomCode, roomState, setStale }

data ServerState = ServerState (MVar (Map RoomCode Room))

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

broadcast :: RoomCode -> Map Username Client -> ToClient -> IO ()
broadcast roomCode clients msg = do
  print (roomCode, "broadcast", msg)
  mapM_ (\Client{ ear } -> writeChan ear msg) clients

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

sendToClient :: RoomCode -> Map Username Client -> Username -> ToClient -> IO ()
sendToClient roomCode clients username msg =
  case Map.lookup username clients of
    Nothing -> print (roomCode, "sendToClient: no client", username)
    Just Client{ ear } -> writeChan ear msg

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
                    room <- newRoom code roomSettings setStale
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

data Message
  = Server String
  | Targeted Username ToClient
  | Broadcast ToClient

sendMessages :: [Message] -> Writer (Endo [Message]) ()
sendMessages = tell . Endo . (++)

getMessages :: Endo [Message] -> [Message]
getMessages (Endo k) = k []

modifyGame
  :: Undoable
  -> Room
  -> (GameState -> Writer (Endo [Message]) (GameState, a))
  -> IO a
modifyGame undoable Room{ roomCode, roomState } change = do
  modifyMVar roomState $ \state@RoomState{ clients, game } -> do
    let
      ((nextState, result), messages) = runWriter (change (latestState game))
      apply = case undoable of
        CanUndo -> addState
        CannotUndo -> setLatestState
    forM_ (getMessages messages) $ \case
      Server s -> print (roomCode, s)
      Targeted u tc -> sendToClient roomCode clients u tc
      Broadcast tc -> broadcast roomCode clients tc
    return (state{ game = apply nextState game }, result)

applyUndo :: Room -> Username -> IO ()
applyUndo Room{ roomCode, roomState } username = do
  modifyMVar_ roomState $ \state@RoomState{ clients, game } ->
    case undo game of
      Nothing -> do
        return state
      Just undone -> do
        let latest@GameState{ board = uBoard, players = uPlayers } = latestState undone
        ifor_ uPlayers $ \uName PlayerState{ rack } ->
          sendToClient roomCode clients uName (UpdateRack rack)
        mapM_ (broadcast roomCode clients)
          [ Scores (scores latest)
          , UpdateBoard uBoard
          , PlayerMoved { movePlayer = username, moveReport = Undone }
          ]
        return state{ game = undone }

addClient :: Maybe Client -> IO (Chan ToClient, Maybe Client)
addClient Nothing = do
  ear <- newChan
  return (ear, Just Client{ connectionCount = 1, ear })
addClient (Just Client{ connectionCount = n, ear = oldEar }) = do
  newEar <- dupChan oldEar
  return (newEar, Just Client{ connectionCount = n + 1, ear = newEar })

removeClient :: Maybe Client -> Maybe Client
removeClient Nothing = Nothing
removeClient (Just Client{ connectionCount = n, ear })
  | n <= 1 = Nothing
  | otherwise = Just Client{ connectionCount = n - 1, ear }

joinedRoom :: Room -> WS.Connection -> Username -> IO ()
joinedRoom room@Room{ roomState, roomCode, setStale } conn username = do
  setStale False
  sendToConn conn (UpdateRoomCode roomCode)
  clientEar <- modifyMVar roomState $ \state@RoomState{ clients } -> do
    (ear, newClients) <- getCompose (Map.alterF (Compose . addClient) username clients)
    return (state{ clients = newClients }, ear)
  modifyGame CannotUndo room $ \gameState -> do
    let withPlayer = addPlayerIfAbsent username gameState
    sendMessages [Broadcast (Scores (scores withPlayer))]
    return (withPlayer, ())
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (playerRead room conn username)
    (writeThread room clientEar conn username)
    `onException` do
      modifyMVar_ roomState $ \state@RoomState{ clients } -> do
        print (roomCode, "disconnected", username)
        let newClients = Map.alter removeClient username clients
        when (Map.null newClients) (setStale True)
        return state{ clients = newClients }
  () <- absurd readDoesNotReturn
  absurd neitherDoesWrite

playerRead :: Room -> WS.Connection -> Username -> IO Void
playerRead room@Room{ roomCode, roomState } conn username = forever $ do
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
          withMVar roomState $ \RoomState{ clients } -> do
            broadcast roomCode clients
              ChatMessage{ chatSentBy = username, chatContent = msgToSend }
    Just (MakeMove move) -> doMove (applyMove username move)
    Just (Exchange tiles) -> doMove (applyExchange username tiles)
    Just Pass -> doMove (applyPass username)
    Just Undo -> applyUndo room username
  where
    doMove applyToGame =
      modifyGame CanUndo room $ \game ->
        case applyToGame game of
          Right (moveReport, nextGame@GameState{ board, players }) -> do
            sendMessages [Targeted username (MoveResult (Right ()))]
            case Map.lookup username players of
              Nothing ->
                sendMessages [Server $ "sendRack: player missing: " ++ show username]
              Just PlayerState{ rack } ->
                sendMessages [Targeted username (UpdateRack rack)]
            sendMessages
              [ Broadcast $ PlayerMoved { movePlayer = username, moveReport }
              , Broadcast $ Scores (scores nextGame)
              , Broadcast $ UpdateBoard board
              ]
            case nextGame of
              GameState{ gameOver = True } ->
                sendMessages
                  [ Broadcast $ Scores (scores nextGame)
                  , Broadcast $ GameOver
                  ]
              _ -> return ()
            return (nextGame, ())
          Left moveError -> do
            sendMessages [Targeted username (MoveResult (Left moveError))]
            return (game, ())

writeThread :: Room -> Chan ToClient -> WS.Connection -> Username -> IO Void
writeThread Room{ roomState, roomCode } ear conn username = do
  withMVar roomState $ \RoomState{ game } -> do
    let GameState{ board, players } = latestState game
    mapM_ (writeChan ear)
      [ UpdateBoard board
      , UpdateTileData tileData
      ]
    case Map.lookup username players of
      Nothing -> print (roomCode, "sendRack: player missing", username)
      Just PlayerState{ rack } -> writeChan ear (UpdateRack rack)
  forever $ do
    sendToConn conn =<< readChan ear
