module Value.ValueType where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), Value, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Expression (Expression)
import GHC.Generics (Generic)
import Scope (Scope)

data ValueType = ValueType
    -- this is the definition of a value field
    { name :: Text
    , expr :: Expression
    , scope :: Scope
    , mandatory :: Bool
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ValueType where
    parseJSON :: JSON.Value -> Parser ValueType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ValueType where
    toJSON :: ValueType -> JSON.Value
    toJSON = genericToJSON defaultOptions
