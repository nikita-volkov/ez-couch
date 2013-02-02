{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module EZCouch.Types where

import Prelude ()
import ClassyPrelude 
import Data.Generics (Data, Typeable)
import Data.Aeson
import GHC.Generics

-- | A wrapper for entity values which preserves the information required for
-- identifying the appropriate documents in the db.
data Persisted a = Persisted { persistedId :: Text, persistedRev :: Text, persistedEntity :: a }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)
instance (ToJSON a) => ToJSON (Persisted a)
instance (FromJSON a) => FromJSON (Persisted a)

persistedIdRev :: Persisted a -> IdRev a
persistedIdRev (Persisted id rev _) = IdRev id rev

type Identified a = (Text, a)
identifiedId (id, _) = id
identifiedEntity (_, entity) = entity

data IdRev a = IdRev Text Text

data EZCouchException 
  = ParsingException Text 
  -- ^ A response from CouchDB could not be parsed.
  | OperationException Text 
  -- ^ An operation failed, e.g. a document couldn't be created or deleted.
  | ServerException Text
  -- ^ E.g., server provided an unexpected response
  | ConnectionException Text
  deriving (Show, Data, Typeable)
instance Exception EZCouchException

