module Process.Reconcile where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Reconciliation = Reconciliation
    -- a Reconciliation is link between a Process (ie an activity) and a partial Event
    -- TODO use newtype style Uuids (phantom type): type Event = Event Uuid
    { qty :: Rational
    , event :: UUID
    , process :: UUID
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Reconciliation where
    parseJSON :: JSON.Value -> Parser Reconciliation
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Reconciliation where
    toJSON :: Reconciliation -> JSON.Value
    toJSON = genericToJSON defaultOptions
