module ResourceType.ResourceType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data ResourceType = ResourceType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ResourceType where
    parseJSON :: JSON.Value -> Parser ResourceType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourceType where
    toJSON :: ResourceType -> JSON.Value
    toJSON = genericToJSON defaultOptions
