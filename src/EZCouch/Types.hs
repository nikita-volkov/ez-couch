{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module EZCouch.Types where

import Prelude ()
import BasicPrelude 

import Data.Generics
import Database.CouchDB.Conduit 

data Persisted a = Persisted { persistedId :: Text, persistedRev :: Text, persistedValue :: a }

data HighException 
  = ParsingException Text 
  | OperationException Text 
  | ResponseException Text 
  deriving (Show, Data, Typeable)
instance Exception HighException

type DB = Text
type View = Text
type DesignAndView = (Text, Text)