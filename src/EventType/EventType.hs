module EventType.EventType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import Expression (Expression)
import GHC.Generics (Generic)
import Scope (Scope)
import Type (Type)

data EventType = EventType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    , providers :: Scope
    , receivers :: Scope
    , flowscope :: Scope
    , qty :: Expression
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON EventType where
    parseJSON :: JSON.Value -> Parser EventType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON EventType where
    toJSON :: EventType -> JSON.Value
    toJSON = genericToJSON defaultOptions