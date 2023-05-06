{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (FromJSON, Options (sumEncoding), SumEncoding (TaggedObject, contentsFieldName, tagFieldName), ToJSON, Value (..), decode, defaultOptions, encode, genericParseJSON, genericToJSON, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Data (Data (toConstr), Typeable)
import Data.Set as Set (Set)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as IO (appendFile)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Ident.Fragment (Fragment)
import Type (Type)

data MessageFlow = Requested | Sent | Processed
    deriving (Eq, Generic, Data, Typeable, Ord, Show)

instance FromJSON MessageFlow where
    parseJSON :: JSON.Value -> Parser MessageFlow
    parseJSON = genericParseJSON defaultOptions

instance ToJSON MessageFlow where
    toJSON :: MessageFlow -> JSON.Value
    toJSON = genericToJSON defaultOptions

-- Message

data Message = Message Metadata Payload
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Message where
    parseJSON :: JSON.Value -> Parser Message
    parseJSON value = do
        meta <- (JSON.parseJSON =<< withObject "Metadata" (.: "meta") value) :: Parser Metadata
        payl <- JSON.parseJSON value :: Parser Payload
        return $ Message meta payl

instance ToJSON Message where
    toJSON :: Message -> JSON.Value
    toJSON (Message m p) = case genericToJSON defaultOptions p of
        JSON.Object o -> JSON.Object $ KeyMap.insert "meta" (genericToJSON defaultOptions m) o
        _ -> JSON.Null

-- Payload

data Payload
    = InitiatedConnection Connection
    | AddedIdentifier Identifier
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Payload where
    parseJSON :: JSON.Value -> Parser Payload
    parseJSON = genericParseJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "what", contentsFieldName = "load"}}

instance ToJSON Payload where
    toJSON :: Payload -> JSON.Value
    toJSON = genericToJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "what", contentsFieldName = "load"}}

-- Connection

data Connection = Connection {lastMessageTime :: POSIXTime, uuids :: Set UUID}
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Connection where
    parseJSON :: JSON.Value -> Parser Connection
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Connection where
    toJSON :: Connection -> JSON.Value
    toJSON = genericToJSON defaultOptions

-- Metadata

data Metadata = Metadata
    { uuid :: UUID
    , when :: POSIXTime
    , flow :: MessageFlow
    }
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Metadata where
    parseJSON :: JSON.Value -> Parser Metadata
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Metadata where
    toJSON :: Metadata -> JSON.Value
    toJSON = genericToJSON defaultOptions

metadata :: Message -> Metadata
metadata (Message m _) = m

payload :: Message -> Payload
payload (Message _ p) = p

-- payloads as class

-- class Payload p where
--    toString :: p -> String
--
-- instance Payload InitiatedConnection where
--    toString :: InitiatedConnection -> String
--    toString _ = "InitiatedConnection"

-- data Payload = AddedIdentifier Identifier | InitiatedConnection Connection

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
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Identifier where
    parseJSON :: JSON.Value -> Parser Identifier
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Identifier where
    toJSON :: Identifier -> JSON.Value
    toJSON = genericToJSON defaultOptions

data Value = Value

data Configuration = Configuration

data GroupLink = GroupLink

data Reconciliation = Reconciliation

{-data Payload
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

isType :: Message -> T.Text -> Bool
isType (Message _ p) t = t == T.pack (show $ toConstr p)

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . (`isType` t))

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
