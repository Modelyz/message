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

recipients :: Message' -> ServiceRegistry -> Set ServiceRole
recipients m sr = fromMaybe Set.empty $ Map.lookup (show $ toConstr m) sr

-- pour le state de store:
-- agregate:: Connection -> State -> State

-- String -> (a -> Payload)

{-
decodage json : on decode vers un type

String → ServiceRole

quand on reoit un message, on doit obtenir des destinataires.  msg → set recipient
un service qui se déclare va donner une association entre son nom et des types de messages pour pouvoir retrouver son nom en fonction des messages. Donc il donne un truc { "role": "ident", "subscriptions": "AddedIdentifier" }
Il faut transformer "AddedIdentifier" en AddedIdentifier qui est un constructeur de Payload. Le state de store stocke l'association entre AddedIdentifier et "ident".
Donc quand un message arrive, on regarde son what, on le transforme en constructeur de payload avec readMaybe, et on utilise le constructeur dans le decodage json de la payload. Et on decode la metadata.

Je dois décoder un message json et aboutir à au bon type Truc Metadata Payload
-}
