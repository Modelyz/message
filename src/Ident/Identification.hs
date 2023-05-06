module Ident.Identification where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data Identification
    = StartsWith String
    | EndsWith String
    | Contains String
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Identification where
    parseJSON :: JSON.Value -> Parser Identification
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Identification where
    toJSON :: Identification -> JSON.Value
    toJSON = genericToJSON defaultOptions
