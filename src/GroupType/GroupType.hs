module GroupType.GroupType where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Tree qualified
import Type (Type)

data GroupType = GroupType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    , unique :: Bool
    , treeType :: Tree.TreeType
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON GroupType where
    parseJSON :: JSON.Value -> Parser GroupType
    parseJSON = genericParseJSON defaultOptions

instance ToJSON GroupType where
    toJSON :: GroupType -> JSON.Value
    toJSON = genericToJSON defaultOptions
