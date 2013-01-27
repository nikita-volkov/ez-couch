{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.DocIsolation where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Doc
import Data.Aeson
import Data.Time

data DocIsolation a
  = DocIsolation { 
      doc :: a,
      since :: UTCTime, 
      duration :: Int 
    }
  deriving (Show, Eq, Generic)
instance (ToJSON a) => ToJSON (DocIsolation a)
instance (FromJSON a) => FromJSON (DocIsolation a)
instance (Doc a) => Doc (DocIsolation a) where
  docType = const "EZCouchDocIsolation"

