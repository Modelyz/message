module Expression.Binary where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Text as T
import GHC.Generics (Generic)

data Operator
    = Add
    | Multiply
    | Or {name :: T.Text, desc :: T.Text, choice :: Either String Bool}
    | Otherwise
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Operator where
    parseJSON :: JSON.Value -> Parser Operator
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Operator where
    toJSON :: Operator -> JSON.Value
    toJSON = genericToJSON defaultOptions
