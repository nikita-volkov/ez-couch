{-# LANGUAGE OverloadedStrings #-}
module EZCouch.BulkPostRequest where

import Prelude ()
import BasicPrelude

import EZCouch.Request
import EZCouch.Types
import qualified EZCouch.Parsing as Parsing

import Database.CouchDB.Conduit 
import Database.CouchDB.Conduit.View.Query
import qualified Data.Aeson.Types as Aeson 
import qualified Data.Aeson.Encode as Aeson
import qualified Data.Aeson.Utils as Aeson
import Data.Conduit
import qualified Data.Conduit.List as Conduit

import qualified Control.Exception.Lifted as Exception
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Vector as Vector

import qualified Data.Text.Encoding as Text

data Operation
  = Create Aeson.Value Aeson.Value
  | Update Aeson.Value Aeson.Value Aeson.Value
  | Delete Aeson.Value Aeson.Value

perform 
  :: (MonadCouch m) 
  => Text -- ^ Database
  -> [Operation] -- ^ Operation
  -> m [Either Aeson.Value (Aeson.Value, Aeson.Value)] -- ^ Results
perform db ops = do
  src <- postUpdate path [] body
  src $= Conduit.map Parsing.rowToIdEither $$ Conduit.consume
  where
    path = mkPath [Text.encodeUtf8 db, "_bulk_docs"]
    body = Aeson.encode $ operationsJSON ops

operationsJSON ops 
  = Aeson.object [("docs", Aeson.Array $ Vector.fromList $ map operationJSON ops)]

operationJSON (Create id a)
  = Aeson.insert [("_id", id)] a
operationJSON (Update id rev a)
  = Aeson.insert [("_id", id), ("_rev", rev)] a
operationJSON (Delete id rev)
  = Aeson.object [("_id", id), ("_rev", rev), ("_deleted", Aeson.Bool True)] 

