module ProcessType.ProcessType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Set (Set)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data ProcessType = ProcessType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    , eventTypes :: Set UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ProcessType where
    parseJSON :: JSON.Value -> Parser ProcessType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ProcessType where
    toJSON :: ProcessType -> JSON.Value
    toJSON = genericToJSON defaultOptions
