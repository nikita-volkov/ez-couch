{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Sweeper where

import Prelude ()
import ClassyPrelude
import qualified Data.Time as Time
import Control.Concurrent
import Data.Aeson
import EZCouch.Time
import EZCouch.Types
import EZCouch.Action
import EZCouch.Entity
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.Try
import EZCouch.View
import EZCouch.Model.EntityIsolation (EntityIsolation)
import qualified EZCouch.Model.EntityIsolation as EntityIsolation
import EZCouch.Isolation
import qualified Util.Logging as Logging


runSweeper = forever $ do
  Logging.logM 0 "EZCouch.Sweeper" $ "Sweeping zombie entity isolations"
  readZombieEntityIsolations >>= releaseIsolations
  liftIO $ threadDelay $ 10 ^ 6 * 60 * 60 * 24 * 2


readZombieEntityIsolations :: (MonadAction m) 
  => m [Persisted EntityIsolation]
readZombieEntityIsolations = do
  now <- readTime
  readEntities
    (ViewByKeys1 (ViewKeyField "till"))
    (KeysSelectionRangeEnd now)
    0
    Nothing
    False

releaseIsolations isolations = do
  createEntitiesWithIds $ map idAndValue isolations
  deleteEntities isolations

idAndValue =
  (EntityIsolation.entityId &&& EntityIsolation.entityValue) . persistedEntity