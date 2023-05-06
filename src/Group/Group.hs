module Group.Group where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Scope (Scope)
import Type (Type)

data Group = Group
    { what :: Type
    , uuid :: UUID
    , type_ :: UUID
    , parent :: Maybe UUID
    , -- The scope defines what a group can contain
      scope :: Scope
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Group where
    parseJSON :: JSON.Value -> Parser Group
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Group where
    toJSON :: Group -> JSON.Value
    toJSON = genericToJSON defaultOptions
