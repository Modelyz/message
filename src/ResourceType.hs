module ResourceType (ResourceType) where

import Data.UUID (UUID)

data ResourceType = ResourceType
  { what :: HType.Type,
    uuid :: UUID,
    parent :: Maybe UUID
  }
