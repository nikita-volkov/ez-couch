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
  | OperationException Text 
  | ResponseException Text 
  deriving (Show, Data, Typeable)
instance Exception EZCouchException


data ReadOptions a k
  = ReadOptions {
      readOptionsKeys :: Maybe [k],
      readOptionsView :: Maybe Text,
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
