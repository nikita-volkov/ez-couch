{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.View where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Doc
import Data.Aeson

data View = View { map :: Text, reduce :: Maybe Text }
  deriving (Show, Eq, Generic)
instance ToJSON View
instance FromJSON View where
  parseJSON = withObject "View" $ \o -> 
    View <$> o .: "map" <*> o .:? "reduce"  

