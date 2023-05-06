module Connection where

import Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Set as Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Connection = Connection {lastMessageTime :: POSIXTime, uuids :: Set UUID}
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Connection where
    parseJSON :: JSON.Value -> Parser Connection
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Connection where
    toJSON :: Connection -> JSON.Value
    toJSON = genericToJSON defaultOptions
