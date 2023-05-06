module ContractType.ContractType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data ContractType = ContractType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ContractType where
    parseJSON :: JSON.Value -> Parser ContractType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ContractType where
    toJSON :: ContractType -> JSON.Value
    toJSON = genericToJSON defaultOptions
