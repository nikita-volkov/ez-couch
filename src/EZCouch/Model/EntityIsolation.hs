{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.EntityIsolation where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import EZCouch.Types
import Data.Aeson
import Data.Time

data EntityIsolation
  = EntityIsolation { 
      entityId :: Text,
      entityValue :: Value, 
      -- entity :: Value, 
      -- ^ A JSON value to simplify internal handling and reduce conversions.
      till :: UTCTime
    }
  deriving (Show, Eq, Generic)
instance ToJSON EntityIsolation
instance FromJSON EntityIsolation
instance Entity EntityIsolation where
  entityType = const "EZCouchEntityIsolation"

