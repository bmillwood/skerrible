{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

portNumber :: (Num n) => n
portNumber = 4170

data Tile =
  Tile
    { tileChar :: Char
    , tileScore :: Integer
    } deriving (Generic, Show)

instance Aeson.FromJSON Tile
instance Aeson.ToJSON Tile

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

data Move =
  Move
    { startPos :: Pos
    , direction :: MoveDirection
    , tiles :: [Tile]
    }
  deriving (Generic, Show)

instance Aeson.FromJSON Move
instance Aeson.ToJSON Move

data MoveError
  = NotPlaying
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | YouDoNotHave [Tile]
  | DoesNotConnect
  | NotAWord [Move]
  deriving (Generic, Show)

instance Aeson.FromJSON MoveError
instance Aeson.ToJSON MoveError

data FromClient
  = LoginRequest { loginRequestName :: Text }
  | Chat { msgToSend :: Text }
  | MakeMove Move
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient
instance Aeson.ToJSON FromClient

data ToClient
  = Folks { loggedInOthers :: Set Text }
  | Message { msgSentBy :: Text, msgContent :: Text }
  | UpdateBoard Board
  | UpdateRack Rack
  | MoveResult (Either MoveError ())
  deriving (Generic, Show)

instance Aeson.FromJSON ToClient
instance Aeson.ToJSON ToClient
