{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.Isolation where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import Data.Aeson
import Data.Time

data Isolation 
  = Isolation { since :: UTCTime }
  deriving (Show, Eq, Generic)
instance ToJSON (Isolation)
instance FromJSON (Isolation)
instance Entity (Isolation) where
  entityType = const "EZCouchIsolation"
