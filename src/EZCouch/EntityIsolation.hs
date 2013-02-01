{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.EntityIsolation where

import Prelude ()
import ClassyPrelude
import qualified Data.Time as Time
import Data.Aeson
import EZCouch.Time
import EZCouch.Types
import EZCouch.Action
import EZCouch.Entity
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.Try
import qualified EZCouch.Model.EntityIsolation as Model
import EZCouch.Isolation
import qualified Util.Logging as Logging

logM lvl = Logging.logM lvl "EZCouch.EntityIsolation"


type Isolation e = Persisted (Persisted e)
isolationEntity = persistedEntity

isolateEntity :: (MonadAction m, Entity e) 
  => Int
  -- ^ A timeout in seconds. If the isolation does not get released when it
  -- passes, it gets considered to be zombie caused by client interrupt, then
  -- when the sweeper daemon hits the next cycle it will release the entity.
  -> Persisted e
  -- ^ The entity to isolate.
  -> m (Maybe (Isolation e))
  -- ^ Either the isolation or nothing if the entity has been already isolated
  -- by concurrent client.
isolateEntity timeout entity = do
  till <- Time.addUTCTime (fromIntegral timeout) <$> readTime
  isolation <- tryOperation $ createEntity $ Model.EntityIsolation entity till
  case isolation of
    Nothing -> return Nothing
    Just (Persisted id rev _) -> do
      deleteEntity entity
      return $ Just (Persisted id rev entity)


-- releaseEntity :: (MonadAction m, Entity e)
--   => Isolation e
--   -> m (Persisted e)
-- releaseEntity isolation = do

