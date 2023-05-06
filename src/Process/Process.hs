module Process.Process where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data Process = Process
    { what :: Type
    , uuid :: UUID
    , type_ :: UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Process where
    parseJSON :: JSON.Value -> Parser Process
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Process where
    toJSON :: Process -> JSON.Value
    toJSON = genericToJSON defaultOptions
