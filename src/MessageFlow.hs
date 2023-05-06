module MessageFlow where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data MessageFlow = Requested | Sent | Processed
    deriving (Eq, Generic, Data, Typeable, Ord, Show)

instance FromJSON MessageFlow where
    parseJSON :: Value -> Parser MessageFlow
    parseJSON = genericParseJSON defaultOptions

instance ToJSON MessageFlow where
    toJSON :: MessageFlow -> Value
    toJSON = genericToJSON defaultOptions
