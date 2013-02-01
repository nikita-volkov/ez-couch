{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.EntityIsolation where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import EZCouch.Types
import Data.Aeson
import Data.Time

data EntityIsolation a
  = EntityIsolation { 
      entity :: Persisted a,
      till :: UTCTime
      -- since :: UTCTime, 
      -- timeout :: Int 
    }
  deriving (Show, Eq, Generic)
instance (ToJSON a) => ToJSON (EntityIsolation a)
instance (FromJSON a) => FromJSON (EntityIsolation a)
instance (Entity a) => Entity (EntityIsolation a) where
  entityType = const "EZCouchEntityIsolation"

