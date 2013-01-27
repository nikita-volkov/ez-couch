{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.DocIsolation where

import Prelude ()
import ClassyPrelude hiding (delete, log)
import qualified Data.Time as Time

import EZCouch.Time
import EZCouch.Types
import EZCouch.Action hiding (log)
import EZCouch.ReadAction
import EZCouch.BulkOperationsAction
import EZCouch.Model.DocIsolation as DocIsolation
import EZCouch.Isolation

import qualified Util.Logging as Logging

log lvl = Logging.log "EZCouch.DocIsolation" lvl

isolateDoc doc@(Persisted docId _ docValue) =
  isolate (60*60) ("EZCouch.DocIsolation.isolateDoc-" ++ docId) $ do
    count <- readCount ((docReadOptions doc) {readOptionsKeys = Just [docId]})
    if count == 0
      then return Nothing
      else do
        time <- readTime
        isolation <- create $ DocIsolation docValue time
        delete doc
        return $ Just isolation
  where
    docReadOptions = const readOptions :: a -> ReadOptions a Text