{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module EZCouch.Types where

import Prelude ()
import ClassyPrelude 
import Data.Generics (Data, Typeable)

data Persisted a = Persisted { persistedId :: Text, persistedRev :: Text, persistedEntity :: a }
  deriving (Show, Data, Typeable, Eq, Ord)


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

