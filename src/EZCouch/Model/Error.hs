{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Model.Error where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import Data.Aeson hiding (Error)

data Error = 
  Error {
    error :: Text, 
    reason :: Maybe Text, 
    stack :: Maybe [Text]
  } 
  deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o -> 
    Error <$> o .: "error" <*> o .:? "reason" <*> o .:? "stack"
