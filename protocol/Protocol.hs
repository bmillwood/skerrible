{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

portNumber :: (Num n) => n
portNumber = 4170

data Tile
  = Letter Char
  | Blank
  deriving (Eq, Generic, Ord, Show)

instance Aeson.FromJSON Tile
instance Aeson.ToJSON Tile
instance Aeson.FromJSONKey Tile
instance Aeson.ToJSONKey Tile

data TileData =
  TileData
    { tileScore :: Integer
    , tileCount :: Integer
    } deriving (Generic, Show)

instance Aeson.FromJSON TileData
instance Aeson.ToJSON TileData

newtype Rack = Rack [Tile]
  deriving (Generic, Show)

instance Aeson.FromJSON Rack
instance Aeson.ToJSON Rack

data Square =
  Square
    { letterMult :: Integer
    , wordMult :: Integer
    , squareTile :: Maybe Tile
    } deriving (Generic, Show)

instance Aeson.FromJSON Square
instance Aeson.ToJSON Square

data Pos = Pos Integer Integer
  deriving (Eq, Generic, Ord, Show)

instance Aeson.FromJSON Pos
instance Aeson.ToJSON Pos

-- these instances are arguably not optimal, but they sure are convenient
instance Aeson.ToJSONKey Pos
instance Aeson.FromJSONKey Pos

newtype Board = Board (Map Pos Square)
  deriving (Generic, Show)

instance Aeson.FromJSON Board
instance Aeson.ToJSON Board

data MoveDirection
  = MoveRight
  | MoveDown
  deriving (Generic, Show)

instance Aeson.FromJSON MoveDirection
instance Aeson.ToJSON MoveDirection

data MoveTile
  = PlaceTile Tile
  | UseBoard
  deriving (Generic, Show)

instance Aeson.FromJSON MoveTile
instance Aeson.ToJSON MoveTile

data Move =
  Move
    { startPos :: Pos
    , direction :: MoveDirection
    , tiles :: [MoveTile]
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Move
instance Aeson.ToJSON Move

data MoveError
  = NotPlaying
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | NoPlacedTiles
  | YouDoNotHave (NonEmpty Tile)
  | FirstMoveNotInCentre
  | NoMultiletterWordsMade
  | DoesNotConnect
  | NotAWord (NonEmpty Move)
  deriving (Generic, Show)

instance Aeson.FromJSON MoveError
instance Aeson.ToJSON MoveError

newtype Username = Username Text
  deriving (Eq, Generic, Ord, Show)

instance Aeson.FromJSON Username
instance Aeson.ToJSON Username
instance Aeson.FromJSONKey Username
instance Aeson.ToJSONKey Username

data FromClient
  = LoginRequest { loginRequestName :: Username }
  | Chat { msgToSend :: Text }
  | MakeMove Move
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient
instance Aeson.ToJSON FromClient

data MoveReport =
  MoveReport
    { moveMadeBy :: Username
    , moveWords :: [String]
    , moveScore :: Integer
    }
  deriving (Generic, Show)

instance Aeson.FromJSON MoveReport
instance Aeson.ToJSON MoveReport

data ToClient
  = Scores (Map Username Integer)
  | ChatMessage { chatSentBy :: Username, chatContent :: Text }
  | PlayerMoved MoveReport
  | UpdateTileData (Map Tile TileData)
  | UpdateBoard Board
  | UpdateRack Rack
  | MoveResult (Either MoveError ())
  deriving (Generic, Show)

instance Aeson.FromJSON ToClient
instance Aeson.ToJSON ToClient
