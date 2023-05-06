module Resource.Resource where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data Resource = Resource
    { what :: Type
    , uuid :: UUID
    , type_ :: UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Resource where
    parseJSON :: JSON.Value -> Parser Resource
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Resource where
    toJSON :: Resource -> JSON.Value
    toJSON = genericToJSON defaultOptions
