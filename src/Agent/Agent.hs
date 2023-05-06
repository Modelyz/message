module Agent.Agent where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data Agent = Agent
    { what :: Type
    , uuid :: UUID
    , type_ :: UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Agent where
    parseJSON :: JSON.Value -> Parser Agent
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Agent where
    toJSON :: Agent -> JSON.Value
    toJSON = genericToJSON defaultOptions
