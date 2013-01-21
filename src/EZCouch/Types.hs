{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module EZCouch.Types where

import Prelude ()
import ClassyPrelude 

import Data.Generics

data Persisted a = Persisted { persistedId :: Text, persistedRev :: Text, persistedValue :: a }
  deriving (Show, Data, Typeable, Eq, Ord)


data EZCouchException 
  = ParsingException Text 
  -- ^ A response from CouchDB could not be parsed.
  | OperationException Text 
  -- ^ An operation failed, e.g. a document couldn't be created or deleted.
  deriving (Show, Data, Typeable)
instance Exception EZCouchException

-- | Identifies a Couch's design and view. The design name is implicitly resolved from the type parameter `a` and becomes the name of this type. The view name however must be specified explicitly.
newtype View a = View { viewName :: Text }
  deriving (Show, Data, Typeable, Eq, Ord)


data ReadOptions a k
  = ReadOptions {
      readOptionsKeys :: Maybe [k],
      readOptionsView :: Maybe (View a),
      readOptionsDescending :: Bool,
      readOptionsLimit :: Maybe Int,
      readOptionsSkip :: Int
    }
  deriving (Show, Data, Typeable, Eq, Ord)
  
readOptions :: ReadOptions a Text
readOptions = ReadOptions Nothing Nothing False Nothing 0


data ConnectionSettings 
  = ConnectionSettings {  
      connectionSettingsHost :: Text,
      connectionSettingsPort :: Int,
      connectionSettingsAuth :: Maybe (Text, Text),
      connectionSettingsDatabase :: Text
    }

defaultPort = 5984 :: Int
