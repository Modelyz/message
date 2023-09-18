{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Agent.Agent (Agent)
import AgentType.AgentType (AgentType)
import Commitment.Commitment (Commitment)
import CommitmentType.CommitmentType (CommitmentType)
import Configuration (Configuration)
import Connection
import Contract.Contract (Contract)
import ContractType.ContractType (ContractType)
import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson (FromJSON, Options (sumEncoding), ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON, withObject, (.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser, SumEncoding (..))
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Data (Data (toConstr), Typeable)
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as IO (appendFile)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID (UUID)
import Event.Event (Event)
import EventType.EventType (EventType)
import GHC.Generics (Generic)
import Group.Group (Group)
import Group.Link as GroupLink (Link)
import GroupType.GroupType (GroupType)
import Ident.Fragment (Fragment)
import Ident.Identifier (Identifier, fragments)
import Ident.IdentifierType (IdentifierType)
import MessageFlow
import Metadata (Metadata, flow, from, when)
import Process.Process (Process)
import Process.Reconcile (Reconciliation)
import ProcessType.ProcessType (ProcessType)
import Resource.Resource (Resource)
import ResourceType.ResourceType (ResourceType)
import Service (Service)
import Value.Value (Value)
import Value.ValueType (ValueType)

data Message = Message Metadata Payload
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Message where
    parseJSON :: JSON.Value -> Parser Message
    parseJSON value = do
        meta <- (JSON.parseJSON =<< withObject "Metadata" (.: "meta") value) :: Parser Metadata
        payl <- (JSON.parseJSON =<< withObject "Payload" (.: "load") value) :: Parser Payload
        return $ Message meta payl

instance ToJSON Message where
    toJSON :: Message -> JSON.Value
    toJSON (Message m p) = JSON.object [("meta", toJSON m), ("load", toJSON p)]

payload :: Message -> Payload
payload (Message _ p) = p

metadata :: Message -> Metadata
metadata (Message m _) = m

-- Payload

data Payload
    = InitiatedConnection Connection
    | AddedResourceType ResourceType
    | RemovedResourceType UUID
    | AddedEventType EventType
    | RemovedEventType UUID
    | AddedAgentType AgentType
    | RemovedAgentType UUID
    | AddedCommitmentType CommitmentType
    | RemovedCommitmentType UUID
    | AddedContractType ContractType
    | RemovedContractType UUID
    | AddedProcessType ProcessType
    | RemovedProcessType UUID
    | AddedResource Resource
    | RemovedResource UUID
    | AddedEvent Event
    | RemovedEvent UUID
    | AddedAgent Agent
    | RemovedAgent UUID
    | AddedCommitment Commitment
    | RemovedCommitment UUID
    | AddedContract Contract
    | RemovedContract UUID
    | AddedProcess Process
    | RemovedProcess UUID
    | AddedIdentifierType IdentifierType
    | ChangedIdentifierType {old :: IdentifierType, new :: IdentifierType}
    | RemovedIdentifierType IdentifierType
    | AddedIdentifier Identifier
    | AddedValueType ValueType
    | ChangedValueType ValueType ValueType
    | RemovedValueType ValueType
    | AddedValue Value
    | Configured Configuration
    | Unconfigured Configuration
    | AddedGroupType GroupType
    | RemovedGroupType UUID
    | DefinedGroup Group
    | RemovedGroup UUID
    | Grouped GroupLink.Link
    | Ungrouped GroupLink.Link
    | Reconciled Reconciliation
    | Unreconciled Reconciliation
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Payload where
    parseJSON :: JSON.Value -> Parser Payload
    parseJSON = genericParseJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "what", contentsFieldName = "load"}}

instance ToJSON Payload where
    toJSON :: Payload -> JSON.Value
    toJSON = genericToJSON $ defaultOptions{sumEncoding = TaggedObject{tagFieldName = "what", contentsFieldName = "load"}}

isType :: Message -> T.Text -> Bool
isType (Message _ p) t = t == T.pack (show $ toConstr p)

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . (`isType` t))

isAfter :: POSIXTime -> Message -> Bool
isAfter t msg = when (metadata msg) > t

getFlow :: Message -> MessageFlow
getFlow = flow . metadata

setFlow :: MessageFlow -> Message -> Message
setFlow flow (Message m p) = Message m{flow = flow} p

setCreator :: Service -> Message -> Message
setCreator origin (Message m p) = Message m{from = List.singleton origin} p

creator :: Message -> Service
creator = head . from . metadata -- maybe reconsider NonEmpty

addVisited :: Service -> Message -> Message
addVisited origin (Message m p) = Message m{from = from m ++ [origin]} p

dropLastVisited :: Message -> Message
dropLastVisited (Message m p) = Message m{from = reverse $ drop 1 $ reverse $ from m} p

lastVisited :: Message -> Service
lastVisited = last . from . metadata -- maybe reconsider NonEmpty

appendMessage :: FilePath -> Message -> IO ()
appendMessage f msg = do
    -- TODO use decodeUtf8' to avoid errors
    IO.appendFile f $! decodeUtf8 (toStrict $ JSON.encode msg) `T.append` "\n"
    putStrLn "Message stored"

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
