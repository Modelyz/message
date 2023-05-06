module Scope where

import Data.Aeson as JSON (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.Data (Data, Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Ident.Identification as Identification (Identification)
import Type (Type)

data Scope
    = -- a scope is the definition of a set of items:
      -- Either an empty set:
      Empty
    | -- or the set of all sets
      Anything
    | -- A set with a single item of type Type:
      IsItem Type UUID
    | -- the set of items of type Type whose type_ or parent is child of a precise user type
      HasUserType Type UUID
    | -- The set of items with a specific concrete type:
      HasType Type
    | -- TODO : need to rethink what is below. Seems not relevant for Ident and Value. Can an entity be of several type?? Or is it useful for search? Maybe we need to implement a search expression like the one in Value and which is different from the scope?
      -- The union of two sets
      And Scope Scope -- entities of both groups
      -- An alternative between two sets.
    | Or Scope Scope -- entities of either group
    -- Everything but the set
    | Not Scope -- entities not in the group
    -- the set of items with a specific identification
    | Identified Identification -- entities identified somehow
    deriving (Generic, Data, Typeable, Show, Eq, Ord)

instance FromJSON Scope where
    parseJSON :: JSON.Value -> Parser Scope
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Scope where
    toJSON :: Scope -> JSON.Value
    toJSON = genericToJSON defaultOptions
