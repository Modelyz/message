module Expression.ValueSelection where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Text qualified as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type (Type)

data ValueSelection
    = SelectedValue Type UUID T.Text
    | UndefinedValue
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ValueSelection where
    parseJSON :: JSON.Value -> Parser ValueSelection
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ValueSelection where
    toJSON :: ValueSelection -> JSON.Value
    toJSON = genericToJSON defaultOptions
