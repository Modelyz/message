module Configuration.Zone where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), Value, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data Zone
    = SmallcardZone
    | MenuZone
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Zone where
    parseJSON :: JSON.Value -> Parser Zone
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Zone where
    toJSON :: Zone -> JSON.Value
    toJSON = genericToJSON defaultOptions
