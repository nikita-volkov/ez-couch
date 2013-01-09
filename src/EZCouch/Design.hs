{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Design where

import Prelude ()
import BasicPrelude
import Data.Text.Encoding
import Data.Generics

import qualified Database.CouchDB.Conduit.Design as Couch
import EZCouch.Types

createOrUpdateView db design viewName viewMap viewReduce = do
  Couch.couchPutView dbB designB viewNameB viewMapB viewReduceB
  where
    dbB = encodeUtf8 db
    designB = encodeUtf8 design
    viewNameB = encodeUtf8 viewName
    viewMapB = encodeUtf8 viewMap
    viewReduceB = encodeUtf8 <$> viewReduce


