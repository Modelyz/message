module Type where

import Data.Aeson (FromJSON, ToJSON (..), defaultOptions, genericParseJSON, genericToJSON, parseJSON)
import Data.Data (Data, Typeable)
import GHC.Generics

data Type = Resource | Event | Agent | Commitment | Contract | Process | Group
    deriving (Generic, Data, Typeable, Show)

instance FromJSON Type where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Type where
    toJSON = genericToJSON defaultOptions
