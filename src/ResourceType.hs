module ResourceType (ResourceType (..)) where

import Data.UUID (UUID)
import Type (Type)

data ResourceType = ResourceType
    { what :: Type
    , uuid :: UUID
    , parent :: Maybe UUID
    }
