module Group.Link where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data Link = Link
    -- the groupable is in the group
    { what :: Type
    , groupable :: UUID
    , group :: UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Link where
    parseJSON :: JSON.Value -> Parser Link
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Link where
    toJSON :: Link -> JSON.Value
    toJSON = genericToJSON defaultOptions
