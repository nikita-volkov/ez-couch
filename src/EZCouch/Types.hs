{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module EZCouch.Types where

import Prelude ()
import ClassyPrelude 
import Data.Aeson
import GHC.Generics
import EZCouch.Entity
import EZCouch.Crash

-- | A wrapper for entity values which preserves the information required for
-- identifying the appropriate documents in the db.
data Persisted a = Persisted { persistedId :: Text, persistedRev :: Text, persistedValue :: a }
  deriving (Show, Typeable, Eq, Ord, Generic)
instance (ToJSON a) => ToJSON (Persisted a)
instance (FromJSON a) => FromJSON (Persisted a)

persistedIdRev :: Persisted a -> IdRev a
persistedIdRev (Persisted id rev _) = IdRev id rev

persistedIdentified :: Persisted a -> Identified a
persistedIdentified (Persisted id _ value) = (id, value)

persistedIdHashPart :: Entity a => Persisted a -> Text
persistedIdHashPart (Persisted id _ value) = 
  fromMaybe (crash $ "Unexpected id: " ++ show id) $ 
    stripPrefix (entityType value ++ "-") id


type Identified a = (Text, a)
identifiedId (id, _) = id
identifiedValue (_, value) = value

data IdRev a = IdRev Text Text

data EZCouchException 
  = ParsingException Text 
  -- ^ A response from CouchDB could not be parsed.
  | OperationException Text 
  -- ^ An operation failed, e.g. a document couldn't be created or deleted.
  | ResponseException Text
  -- ^ E.g., server provided an unexpected response
  | ConnectionException Text
  -- ^ Either a connection got closed or a timeout passed
  | ServerException Text
  -- ^ A weird status 500 response
  deriving (Show, Typeable)
instance Exception EZCouchException

