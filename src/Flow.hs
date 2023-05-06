module Flow where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Resource.Resource (Resource)
import ResourceType.ResourceType (ResourceType)

data Flow
    = ResourceFlow Resource
    | ResourceTypeFlow ResourceType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Flow where
    parseJSON :: JSON.Value -> Parser Flow
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Flow where
    toJSON :: Flow -> JSON.Value
    toJSON = genericToJSON defaultOptions
