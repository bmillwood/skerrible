{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics

portNumber :: (Num n) => n
portNumber = 4170

data Tile =
  Tile
    { tileChar :: Char
    , tileScore :: Integer
    } deriving (Generic, Show)

instance Aeson.FromJSON Tile
instance Aeson.ToJSON Tile

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

data FromClient
  = LoginRequest { loginRequestName :: Text }
  | Chat { msgToSend :: Text }
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient
instance Aeson.ToJSON FromClient

data ToClient
  = Folks { loggedInOthers :: Set Text }
  | Message { msgSentBy :: Text, msgContent :: Text }
  | UpdateBoard Board
  deriving (Generic, Show)

instance Aeson.FromJSON ToClient
instance Aeson.ToJSON ToClient
