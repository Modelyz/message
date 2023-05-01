{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (FromJSON, Options (sumEncoding), Result (..), SumEncoding (TaggedObject, contentsFieldName, tagFieldName), Value (..), decode, defaultOptions, encode, fromJSON, genericParseJSON, parseJSON, toJSON)
import Data.Aeson.KeyMap as KeyMap (alterF, lookup)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.Scientific
import Data.Text qualified as T (Text, unpack)
import Data.Text.Lazy qualified as LT (Text, append, intercalate, lines)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.IO qualified as IO (appendFile, readFile)
import Data.Time.Clock.POSIX
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import GHC.Generics
import Ident.Fragment
import Type (Type)

type Message = JSON.Value

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

-- Message

data MessageFlow = Requested | Sent | Processed
  deriving (Generic, Data, Typeable, Show)

instance FromJSON MessageFlow where
  parseJSON :: JSON.Value -> Parser MessageFlow
  parseJSON = genericParseJSON defaultOptions

data Message'
  = InitiatedConnection Metadata Connection
  | AddedIdentifier Metadata Identifier
  deriving (Generic, Data, Typeable, Show)

instance FromJSON Message' where
  parseJSON = genericParseJSON $ defaultOptions {sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "load"}}

newtype Connection = Connection {lastMessageTime :: POSIXTime}
  deriving (Generic, Data, Typeable, Show, FromJSON)

-- Metadata

data Metadata = Metadata
  { uuid :: UUID,
    when :: POSIXTime,
    flow :: MessageFlow
  }
  deriving (Generic, Data, Typeable, Show)

instance FromJSON Metadata where
  parseJSON = genericParseJSON defaultOptions

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
  { what :: Type,
    for :: UUID,
    name :: String,
    fragments :: [Fragment]
  }
  deriving (Generic, Data, Typeable, Show)

instance FromJSON Identifier where
  parseJSON = genericParseJSON defaultOptions

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
isType t ev = getString "what" ev == Just t

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . isType t)

isAfter :: Time -> Message -> Bool
isAfter t ev =
  case ev of
    (JSON.Object o) -> case KeyMap.lookup "meta" o of
      Just m -> case getInt "posixtime" m of
        Just et -> et >= t
        Nothing -> False
      _ -> False
    _ -> False

getInt :: KeyMap.Key -> Message -> Maybe Int
getInt k e =
  case e of
    (JSON.Object o) -> case KeyMap.lookup k o of
      Just (JSON.Number n) -> Just n >>= toBoundedInteger
      _ -> Nothing
    _ -> Nothing

getMessages :: JSON.Value -> [Message]
getMessages e =
  -- TODO use Vector instead
  case e of
    (JSON.Object o) -> case KeyMap.lookup "messages" o of
      (Just (JSON.Array a)) -> Vector.toList a
      _ -> []
    _ -> []

getMetaString :: KeyMap.Key -> Message -> Maybe T.Text
getMetaString k e =
  case e of
    (JSON.Object o) -> case KeyMap.lookup "meta" o of
      (Just (JSON.Object m)) -> case KeyMap.lookup k m of
        Just (JSON.String s) -> Just s
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

getFlow :: Message -> Maybe T.Text
getFlow = getMetaString "flow"

isProcessed :: Message -> Bool
isProcessed msg = maybe False ((==) "Processed" . T.unpack) (getMetaString "flow" msg)

getString :: KeyMap.Key -> Message -> Maybe T.Text
getString k e =
  case e of
    (JSON.Object o) -> case KeyMap.lookup k o of
      Just (JSON.String s) -> Just s
      _ -> Nothing
    _ -> Nothing

getUuids :: Message -> [Uuid]
getUuids e =
  case e of
    (JSON.Object o) -> case KeyMap.lookup "load" o of
      (Just (JSON.Object m)) ->
        case KeyMap.lookup "uuids" m of
          Just (JSON.Array uuids) ->
            Vector.toList
              ( fmap
                  ( \case
                      JSON.String uuid -> T.unpack uuid
                      _ -> ""
                  )
                  uuids
              )
          _ -> []
      _ -> []
    _ -> []

setFlow :: T.Text -> Message -> Message
setFlow flow e =
  case e of
    JSON.Object keymap ->
      JSON.toJSON $
        KeyMap.alterF
          ( \case
              Just (JSON.Object meta) ->
                Just $
                  Just $
                    JSON.toJSON $
                      KeyMap.alterF
                        ( \case
                            Just _ -> Just $ Just $ JSON.String flow
                            Nothing -> Nothing
                        )
                        "flow"
                        meta
              Just other -> Just $ Just other
              Nothing -> Nothing
          )
          "meta"
          keymap
    _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
appendMessage :: FilePath -> Message -> IO ()
appendMessage f message =
  IO.appendFile f $ decodeUtf8 (JSON.encode message) `LT.append` "\n"

-- read the message store
readMessages :: FilePath -> IO [Message]
readMessages f =
  do
    es <- catch (IO.readFile f) handleMissing
    case JSON.decode $ encodeUtf8 $ "[" `LT.append` LT.intercalate "," (LT.lines es) `LT.append` "]" of
      Just evs -> return evs
      Nothing -> return []
  where
    handleMissing :: SomeException -> IO LT.Text
    handleMissing (SomeException _) = do
      putStrLn "Could not read MessageSource"
      return ""

getFragments :: Message -> [Fragment]
getFragments msg =
  case msg of
    (JSON.Object o) -> case KeyMap.lookup "load" o of
      (Just (JSON.Object m)) ->
        case KeyMap.lookup "fragments" m of
          Just (JSON.Array fragments) -> do
            result <- fromJSON <$> Vector.toList fragments
            case result of
              Success f -> [f]
              Error _ -> []
          _ -> []
      _ -> []
    _ -> []

setFragments :: [Fragment] -> Message -> Message
setFragments fragments message =
  case message of
    JSON.Object keymap ->
      JSON.toJSON $
        alterF
          ( \case
              (Just (JSON.Object m)) ->
                Just $
                  Just $
                    JSON.toJSON $
                      alterF
                        ( \case
                            (Just _) -> Just $ Just $ JSON.Array $ Vector.fromList $ fmap toJSON fragments
                            _ -> Nothing
                        )
                        "fragments"
                        m
              _ -> Nothing
          )
          "load"
          keymap
    _ -> message
