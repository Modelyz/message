module Type where

import Data.Aeson as JSON (FromJSON, defaultOptions, genericParseJSON, parseJSON)
import Data.Data (Data, Typeable)
import GHC.Generics

data Type = Resource | Event | Agent | Commitment | Contract | Process | Group
  deriving (Generic, Data, Typeable, Show)

instance FromJSON Type where
  parseJSON = genericParseJSON defaultOptions
