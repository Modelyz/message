module Expression.HardLink where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data HardLink
    = -- automatically chosen depending on the context ie the selected scope
      ResourceLink ResourceLink
    | EventLink EventLink
    | AgentLink AgentLink
    | CommitmentLink CommitmentLink
    | ContractLink ContractLink
    | ProcessLink ProcessLink
    | GroupLink GroupLink
    | ResourceTypeLink ResourceTypeLink
    | EventTypeLink EventTypeLink
    | AgentTypeLink AgentTypeLink
    | CommitmentTypeLink CommitmentTypeLink
    | ContractTypeLink ContractTypeLink
    | ProcessTypeLink ProcessTypeLink
    | GroupTypeLink GroupTypeLink
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON HardLink where
    parseJSON :: JSON.Value -> Parser HardLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON HardLink where
    toJSON :: HardLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ResourceLink
    = -- Groups of type Uuid
      ResourceGroup
    | -- ResourceTypes of type Uuid
      ResourceType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ResourceLink where
    parseJSON :: JSON.Value -> Parser ResourceLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourceLink where
    toJSON :: ResourceLink -> JSON.Value
    toJSON = genericToJSON defaultOptions
data EventLink
    = EventProvider
    | EventReceiver
    | EventInflow
    | EventOutflow
    | EventGroup
    | EventType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON EventLink where
    parseJSON :: JSON.Value -> Parser EventLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON EventLink where
    toJSON :: EventLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data AgentLink
    = AgentGroup
    | AgentType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON AgentLink where
    parseJSON :: JSON.Value -> Parser AgentLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON AgentLink where
    toJSON :: AgentLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data CommitmentLink
    = CommitmentProvider
    | CommitmentReceiver
    | CommitmentInflow
    | CommitmentOutflow
    | CommitmentGroup
    | CommitmentType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON CommitmentLink where
    parseJSON :: JSON.Value -> Parser CommitmentLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON CommitmentLink where
    toJSON :: CommitmentLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ContractLink
    = ContractGroup
    | ContractType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ContractLink where
    parseJSON :: JSON.Value -> Parser ContractLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ContractLink where
    toJSON :: ContractLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ProcessLink
    = ProcessGroup
    | ProcessType
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ProcessLink where
    parseJSON :: JSON.Value -> Parser ProcessLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ProcessLink where
    toJSON :: ProcessLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data GroupLink
    = GroupGroup
    | GroupType
    | ParentGroup
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON GroupLink where
    parseJSON :: JSON.Value -> Parser GroupLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON GroupLink where
    toJSON :: GroupLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ResourceTypeLink
    = ResourceTypeGroup
    | ResourceTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ResourceTypeLink where
    parseJSON :: JSON.Value -> Parser ResourceTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourceTypeLink where
    toJSON :: ResourceTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data EventTypeLink
    = EventTypeProvider
    | EventTypeReceiver
    | EventTypeInflow
    | EventTypeOutflow
    | EventTypeGroup
    | EventTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON EventTypeLink where
    parseJSON :: JSON.Value -> Parser EventTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON EventTypeLink where
    toJSON :: EventTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data AgentTypeLink
    = AgentTypeGroup
    | AgentTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON AgentTypeLink where
    parseJSON :: JSON.Value -> Parser AgentTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON AgentTypeLink where
    toJSON :: AgentTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data CommitmentTypeLink
    = CommitmentTypeProvider
    | CommitmentTypeReceiver
    | CommitmentTypeInflow
    | CommitmentTypeOutflow
    | CommitmentTypeGroup
    | CommitmentTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON CommitmentTypeLink where
    parseJSON :: JSON.Value -> Parser CommitmentTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON CommitmentTypeLink where
    toJSON :: CommitmentTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ContractTypeLink
    = ContractTypeGroup
    | ContractTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ContractTypeLink where
    parseJSON :: JSON.Value -> Parser ContractTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ContractTypeLink where
    toJSON :: ContractTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data ProcessTypeLink
    = ProcessTypeGroup
    | ProcessTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON ProcessTypeLink where
    parseJSON :: JSON.Value -> Parser ProcessTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ProcessTypeLink where
    toJSON :: ProcessTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions

data GroupTypeLink
    = GroupTypeGroup
    | GroupTypeParent
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON GroupTypeLink where
    parseJSON :: JSON.Value -> Parser GroupTypeLink
    parseJSON = genericParseJSON defaultOptions

instance ToJSON GroupTypeLink where
    toJSON :: GroupTypeLink -> JSON.Value
    toJSON = genericToJSON defaultOptions
