{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.Design where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import Data.Aeson
import qualified EZCouch.Model.View as ViewModel

data Design a 
  = Design {
      views :: Map Text ViewModel.View
    }
  deriving (Show, Eq, Generic)
instance ToJSON (Design a)
instance FromJSON (Design a)
instance (Entity a) => Entity (Design a)

designName = docType . (undefined :: Design a -> a)
