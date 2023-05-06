module Expression where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Expression.Binary as B
import Expression.Observable (Observable)
import Expression.Unary as U
import GHC.Generics (Generic)

data Expression
    = Leaf Observable
    | Unary U.Operator Expression
    | Binary B.Operator Expression Expression
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Expression where
    parseJSON :: JSON.Value -> Parser Expression
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Expression where
    toJSON :: Expression -> JSON.Value
    toJSON = genericToJSON defaultOptions
