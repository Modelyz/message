module Value.Value where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON (Value)
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.UUID (UUID)
import Expression (Expression)
import GHC.Generics (Generic)
import Type (Type)

data Value = Value
    { what :: Type
    , for :: UUID
    , name :: Text
    , expr :: Expression
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Value where
    parseJSON :: JSON.Value -> Parser Value
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Value where
    toJSON :: Value -> JSON.Value
    toJSON = genericToJSON defaultOptions
