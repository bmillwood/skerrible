{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import qualified Data.Aeson as Aeson
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics

portNumber :: (Num n) => n
portNumber = 45286

data FromClient
  = LoginRequest { loginRequestName :: Text }
  | Chat { msgToSend :: Text }
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient
instance Aeson.ToJSON FromClient

data ToClient
  = Folks { loggedInOthers :: Set Text }
  | Message { msgSentBy :: Text, msgContent :: Text }
  deriving (Generic, Show)

instance Aeson.FromJSON ToClient
instance Aeson.ToJSON ToClient
