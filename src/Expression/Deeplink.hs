module Expression.Deeplink where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Expression.HardLink as HardLink (HardLink)
import GHC.Generics (Generic)
import Scope (Scope)

data DeepLink
    = Null
    | Link HardLink DeepLink
    | -- the endpoint corresponds to a ValueType
      -- the EndPoint scope is the restriction given by the last hardlink destination
      EndPoint Scope String
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON DeepLink where
    parseJSON :: JSON.Value -> Parser DeepLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON DeepLink where
    toJSON :: DeepLink -> JSON.Value
    toJSON = genericToJSON defaultOptions
