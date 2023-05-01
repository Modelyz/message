{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (FromJSON, Options (sumEncoding), SumEncoding (TaggedObject, contentsFieldName, tagFieldName), ToJSON, Value (..), decode, defaultOptions, encode, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Data (Data (toConstr), Typeable)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as IO (appendFile)
import Data.Time.Clock.POSIX
import Data.UUID (UUID)
import GHC.Generics
import Ident.Fragment
import Type (Type)

type Uuid = String -- uuid as a string generated in Elm

-- Message

data MessageFlow = Requested | Sent | Processed
    deriving (Eq, Generic, Data, Typeable, Show)

instance FromJSON MessageFlow where
    parseJSON :: JSON.Value -> Parser MessageFlow
    parseJSON = genericParseJSON defaultOptions

instance ToJSON MessageFlow where
    toJSON = genericToJSON defaultOptions

data Message = Message Metadata Payload
    deriving (Generic, Data, Typeable, Show)

instance FromJSON Payload where
    parseJSON :: JSON.Value -> Parser Payload
    parseJSON = genericParseJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "load"}}

instance ToJSON Payload where
    toJSON = genericToJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "load"}}

data Payload
    = InitiatedConnection Connection
    | AddedIdentifier Identifier
    deriving (Generic, Data, Typeable, Show)

instance FromJSON Message where
    parseJSON :: JSON.Value -> Parser Message
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Message where
    toJSON = genericToJSON defaultOptions

newtype Connection = Connection {lastMessageTime :: POSIXTime}
    deriving (Generic, Data, Typeable, Show, FromJSON, ToJSON)

-- Metadata

data Metadata = Metadata
    { uuid :: UUID
    , when :: POSIXTime
    , flow :: MessageFlow
    }
    deriving (Generic, Data, Typeable, Show)

instance FromJSON Metadata where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Metadata where
    toJSON = genericToJSON defaultOptions

metadata :: Message -> Metadata
metadata (Message m _) = m

payload :: Message -> Payload
payload (Message _ p) = p

-- payloads

-- class Payload p where
--    toString :: p -> String
--
-- instance Payload InitiatedConnection where
--    toString :: InitiatedConnection -> String
--    toString _ = "InitiatedConnection"

-- data Payload = AddedIdentifier Identifier | InitiatedConnection Connection

{-plop :: String -> Either String (a -> Payload)
plop m =
  case m of
    "InitiatedConnection" -> Right InitiatedConnection
    "AddedResourceType" -> Right AddedResourceType
    "RemovedResourceType" -> Right RemovedResourceType
    "AddedEventType" -> Right AddedEventType
    "RemovedEventType" -> Right RemovedEventType
    "AddedAgentType" -> Right AddedAgentType
    "RemovedAgentType" -> Right RemovedAgentType
    "AddedCommitmentType" -> Right AddedCommitmentType
    "RemovedCommitmentType" -> Right RemovedCommitmentType
    "AddedContractType" -> Right AddedContractType
    "RemovedContractType" -> Right RemovedContractType
    "AddedProcessType" -> Right AddedProcessType
    "RemovedProcessType" -> Right RemovedProcessType
    "AddedResource" -> Right AddedResource
    "RemovedResource" -> Right RemovedResource
    "AddedEvent" -> Right AddedEvent
    "RemovedEvent" -> Right RemovedEvent
    "AddedAgent" -> Right AddedAgent
    "RemovedAgent" -> Right RemovedAgent
    "AddedCommitment" -> Right AddedCommitment
    "RemovedCommitment" -> Right RemovedCommitment
    "AddedContract" -> Right AddedContract
    "RemovedContract" -> Right RemovedContract
    "AddedProcess" -> Right AddedProcess
    "RemovedProcess" -> Right RemovedProcess
    "AddedIdentifierType" -> Right AddedIdentifierType
    "ChangedIdentifierType" -> Right ChangedIdentifierType
    "RemovedIdentifierType" -> Right RemovedIdentifierType
    "AddedIdentifier" -> Right AddedIdentifier
    "AddedValueType" -> Right AddedValueType
    "ChangedValueType" -> Right ChangedValueType
    "RemovedValueType" -> Right RemovedValueType
    "AddedValue" -> Right AddedValue
    "Configured" -> Right Configured
    "Unconfigured" -> Right Unconfigured
    "AddedGroupType" -> Right AddedGroupType
    "RemovedGroupType" -> Right RemovedGroupType
    "DefinedGroup" -> Right DefinedGroup
    "RemovedGroup" -> Right RemovedGroup
    "Grouped" -> Right Grouped
    "Ungrouped" -> Right Ungrouped
    "Reconciled" -> Right Reconciled
    "Unreconciled" -> Right Unreconciled
    _ -> Left "Unknown Message Type"
-}

data ResourceType = ResourceType

data EventType = EventType

data AgentType = AgentType

data CommitmentType = CommitmentType

data ContractType = ContractType

data ProcessType = ProcessType

data GroupType = GroupType

{-
data Resource = Resource

data Event = Event

data Agent = Agent

data Commitment = Commitment

data Process = Process

data Group = Groupe

data Contract = Contract
-}
data IdentifierType = IdentifierType

data ValueType = ValueType

data Identifier = Identifier
    { what :: Type
    , for :: UUID
    , name :: String
    , fragments :: [Fragment]
    }
    deriving (Generic, Data, Typeable, Show)

instance FromJSON Identifier where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Identifier where
    toJSON = genericToJSON defaultOptions

data Value = Value

data Configuration = Configuration

data GroupLink = GroupLink

data Reconciliation = Reconciliation

{-data Payload2
    = InitiatedConnection Connection
    | AddedResourceType ResourceType
    | RemovedResourceType Uuid
    | AddedEventType EventType
    | RemovedEventType Uuid
    | AddedAgentType AgentType
    | RemovedAgentType Uuid
    | AddedCommitmentType CommitmentType
    | RemovedCommitmentType Uuid
    | AddedContractType ContractType
    | RemovedContractType Uuid
    | AddedProcessType ProcessType
    | RemovedProcessType Uuid
    | AddedResource Resource
    | RemovedResource Uuid
    | AddedEvent Event
    | RemovedEvent Uuid
    | AddedAgent Agent
    | RemovedAgent Uuid
    | AddedCommitment Commitment
    | RemovedCommitment Uuid
    | AddedContract Contract
    | RemovedContract Uuid
    | AddedProcess Process
    | RemovedProcess Uuid
    | AddedIdentifierType IdentifierType
    | ChangedIdentifierType IdentifierType IdentifierType
    | RemovedIdentifierType IdentifierType
    | AddedIdentifier Identifier
    | AddedValueType ValueType
    | ChangedValueType ValueType ValueType
    | RemovedValueType ValueType
    | AddedValue Value
    | Configured Configuration
    | Unconfigured Configuration
    | AddedGroupType GroupType
    | RemovedGroupType Uuid
    | DefinedGroup Group
    | RemovedGroup Uuid
    | Grouped GroupLink
    | Ungrouped GroupLink
    | Reconciled Reconciliation
    | Unreconciled Reconciliation
-}

isType :: T.Text -> Message -> Bool
isType t (Message _ p) = t == T.pack (show $ toConstr p)

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . isType t)

isAfter :: POSIXTime -> Message -> Bool
isAfter t msg = when (metadata msg) > t

getFlow :: Message -> MessageFlow
getFlow = flow . metadata

isProcessed :: Message -> Bool
isProcessed msg = getFlow msg == Processed

setFlow :: MessageFlow -> Message -> Message
setFlow flow (Message m p) = Message m{flow = flow} p

appendMessage :: FilePath -> Message -> IO ()
appendMessage f message =
    -- TODO use decodeUtf8' to avoid errors
    IO.appendFile f $ decodeUtf8 (toStrict $ JSON.encode message) `T.append` "\n"

-- read the message store
readMessages :: FilePath -> IO [Message]
readMessages f =
    do
        es <- catch (LBS.readFile f) handleError
        case mapM JSON.decode (LBS.lines es) of
            Just evs -> return evs
            Nothing -> return []
  where
    handleError :: SomeException -> IO LBS.ByteString
    handleError (SomeException _) = do
        putStrLn "Could not read MessageSource"
        return ""

getFragments :: Message -> [Fragment]
getFragments (Message _ p) = case p of
    AddedIdentifier i -> fragments i
    _ -> []

setFragments :: [Fragment] -> Message -> Message
setFragments fragments (Message m p) = case p of
    AddedIdentifier i -> Message m $ AddedIdentifier i{fragments = fragments}
    _ -> Message m p
