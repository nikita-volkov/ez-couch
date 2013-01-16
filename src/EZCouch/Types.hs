{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module EZCouch.Types where

import Prelude ()
import BasicPrelude 

import Data.Generics
import Database.CouchDB.Conduit 


data Persisted a = Persisted { persistedId :: ByteString, persistedRev :: ByteString, persistedValue :: a }
  deriving (Show, Data, Typeable, Eq, Ord)


data EZCouchException 
  = ParsingException Text 
  | OperationException Text 
  | ResponseException Text 
  deriving (Show, Data, Typeable)
instance Exception EZCouchException


data ReadOptions a k
  = ReadOptions {
      readOptionsKeys :: Maybe [k],
      readOptionsView :: Maybe ByteString,
      readOptionsDescending :: Bool,
      readOptionsLimit :: Maybe Int,
      readOptionsSkip :: Int
    }
  deriving (Show, Data, Typeable, Eq, Ord)
  
readOptions :: ReadOptions a ByteString
readOptions = ReadOptions Nothing Nothing False Nothing 0


data ConnectionSettings 
  = ConnectionSettings {  
      connectionSettingsHost :: ByteString,
      connectionSettingsPort :: Int,
      connectionSettingsAuth :: Maybe (ByteString, ByteString),
      connectionSettingsDatabase :: ByteString
    }

defaultPort = 5984 :: Int
