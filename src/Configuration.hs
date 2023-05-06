module Configuration where

import Configuration.Zone (Zone)
import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), Value, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Ident.Fragment (Fragment)
import Scope (Scope)
import Type (Type)

data Configuration
    = -- the list of identifier types to display on each zone
      ZoneDisplay Zone [Fragment] Scope
    | MenuDisplay Type UUID Bool
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Configuration where
    parseJSON :: JSON.Value -> Parser Configuration
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Configuration where
    toJSON :: Configuration -> JSON.Value
    toJSON = genericToJSON defaultOptions
