module AgentType.AgentType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data AgentType = AgentType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON AgentType where
    parseJSON :: JSON.Value -> Parser AgentType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON AgentType where
    toJSON :: AgentType -> JSON.Value
    toJSON = genericToJSON defaultOptions
