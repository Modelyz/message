module Service (recipients, ServiceRole, ServiceRegistry) where

import Data.Data (toConstr)
import Data.Map.Strict as Map (Map, lookup)
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, empty)
import Message

type MessageType = String
type ServiceRole = String

type ServiceRegistry =
    -- The list of services, and the messages accepted by each service
    Map MessageType (Set ServiceRole)

recipients :: Message -> ServiceRegistry -> Set ServiceRole
recipients m sr = fromMaybe Set.empty $ Map.lookup (show $ toConstr $ payload m) sr
