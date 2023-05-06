module Event.Event where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID (UUID)
import Expression (Expression)
import Flow (Flow)
import GHC.Generics (Generic)
import Type (Type)

data Event = Event
    { what :: Type
    , uuid :: UUID
    , when :: POSIXTime
    , qty :: Expression
    , type_ :: UUID
    , provider :: UUID
    , receiver :: UUID
    , flow :: Flow
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Event where
    parseJSON :: JSON.Value -> Parser Event
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Event where
    toJSON :: Event -> JSON.Value
    toJSON = genericToJSON defaultOptions
