{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)

portNumber :: (Num n) => n
portNumber = 4170

data Tile
  = Letter Char
  | Blank
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass
    ( Aeson.FromJSON, Aeson.ToJSON
    , Aeson.FromJSONKey, Aeson.ToJSONKey
    )

data TileData
  = TileData
    { tileScore :: Integer
    , tileCount :: Integer
    }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype Rack = Rack [Tile]
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Square
  = Square
    { letterMult :: Integer
    , wordMult :: Integer
    , squareTile :: Maybe Tile
    }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Pos = Pos Integer Integer
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass
    ( Aeson.FromJSON, Aeson.ToJSON
    -- these instances are arguably not optimal, but they sure are convenient
    , Aeson.FromJSONKey, Aeson.ToJSONKey
    )

newtype Board = Board (Map Pos Square)
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data MoveDirection
  = MoveRight
  | MoveDown
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data MoveTile
  = PlaceTile Tile
  | UseBoard
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Move
  = Move
    { startPos :: Pos
    , direction :: MoveDirection
    , tiles :: [MoveTile]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data MoveError
  = YouAreNotPlaying
  | GameIsOver
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | NoPlacedTiles
  | YouDoNotHave (NonEmpty Tile)
  | FirstMoveNotInCentre
  | NoMultiletterWordsMade
  | DoesNotConnect
  | NotAWord (NonEmpty Move)
  | NotEnoughTilesToExchange
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype Username = Username Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass
    ( Aeson.FromJSON, Aeson.ToJSON
    , Aeson.FromJSONKey, Aeson.ToJSONKey
    )

newtype RoomCode = RoomCode Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data TurnEnforcement
  = NoEnforcement
  | LetPlayersChoose
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data RoomSettings
  = RoomSettings
    { noBoardMultipliers :: Bool
    , turnEnforcement :: TurnEnforcement
    }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data RoomSpec
  = JoinRoom RoomCode
  | MakeNewRoom RoomSettings
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data FromClient
  = LoginRequest { loginRequestName :: Username, roomSpec :: RoomSpec }
  | JoinGame
  | StartNewGame
  | Chat { msgToSend :: Text }
  | MakeMove Move
  | Exchange (NonEmpty Tile)
  | Pass
  | Undo
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data TechErrorMsg
  = ProtocolError
  | MustNotBeEmpty
  | TooLong { lengthUsed :: Integer, lengthLimit :: Integer }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

checkTooLong :: Text -> Integer -> Maybe TechErrorMsg
checkTooLong t lengthLimit
  | lengthUsed == 0 = Just MustNotBeEmpty
  | lengthUsed > lengthLimit = Just TooLong{ lengthUsed, lengthLimit }
  | otherwise = Nothing
  where
    lengthUsed = toInteger (Text.length t)

validUsername :: Username -> Maybe TechErrorMsg
validUsername (Username u) = checkTooLong u 80

validChat :: Text -> Maybe TechErrorMsg
validChat t = checkTooLong t 1000

data MoveReport
  = PlayedWord
    { moveWords :: [String]
    , moveScore :: Integer
    }
  | Exchanged Integer
  | Passed
  | Undone
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data ToClient
  = TechnicalError TechErrorMsg
  | RoomDoesNotExist
  | UpdateRoomCode RoomCode
  | People (Set Username)
  | Scores (Map Username Integer)
  | ChatMessage { chatSentBy :: Username, chatContent :: Text }
  | PlayerMoved { movePlayer :: Username, moveReport :: MoveReport }
  | UpdateTileData (Map Tile TileData)
  | UpdateBoard Board
  | UpdateRack Rack
  | MoveResult (Either MoveError ())
  | GameOver
  | NewGameStarted { gameStartedBy :: Username }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
