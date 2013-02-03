{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.View where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import Data.Aeson

data View = View { map :: Text, reduce :: Maybe Text }
  deriving (Show, Eq, Generic)
instance ToJSON View where
  toJSON (View map reduce) = object $ catMaybes 
    [ Just ("map", toJSON map), 
      (,) <$> pure "reduce" <*> toJSON <$> reduce ]
instance FromJSON View where
  parseJSON = withObject "View" $ \o -> 
    View <$> o .: "map" <*> o .:? "reduce"  

