{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.EntityIsolation where

import Prelude ()
import ClassyPrelude
import qualified Data.Traversable as Traversable
import qualified Data.List as List
import qualified Data.Time as Time
import Data.Aeson
import EZCouch.Time
import EZCouch.Types
import EZCouch.Action
import EZCouch.Entity
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.Try
import EZCouch.Logging
import qualified EZCouch.Model.EntityIsolation as Model

data Isolation e = Isolation {
  isolationIdRev :: IdRev Model.EntityIsolation,
  isolationIdentified :: Identified e
}
isolationEntity = identifiedValue . isolationIdentified

-- | Protect the entity from being accessed by concurrent clients until you 
-- release it using `releaseIsolation`, delete it with the isolation using 
-- `deleteIsolation`, or the timeout passes and it gets considered to be zombie 
-- and gets released automatically some time later.
-- 
-- The automatic releasing gets done by a sweeper daemon running in background
-- when EZCouch is being used on a timely basis and on its launch.
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
isolateEntity timeout persisted = do 
  results <- isolateEntities timeout . singleton $ persisted
  case results of
    [result] -> return result
    _ -> throwIO $ ResponseException $ "EZCouch.EntityIsolation.isolateEntity"

-- | Does the same as `isolateEntity` but for multiple entities and in a single
-- request.
isolateEntities :: (MonadAction m, Entity e)
  => Int
  -> [Persisted e]
  -> m ([Maybe (Isolation e)])
isolateEntities timeout entities = do
  till <- Time.addUTCTime (fromIntegral timeout) <$> readTime
  results <- createIdentifiedEntities
    $ map (entityIsolationId &&& entityIsolationModel till) entities
  results <- return 
    $ List.zipWith (\e r -> (,) <$> pure e <*> r) entities results
  deleteEntities $ map fst $ rights results
  forM results $ \r -> case r of
    Right (entity, isolation) -> do
      return $ Just
        $ Isolation (persistedIdRev isolation) (persistedIdentified entity)
    _ -> return Nothing

entityIsolationModel :: (Entity e) 
  => Time.UTCTime -> Persisted e -> Model.EntityIsolation
entityIsolationModel till entity =
  Model.EntityIsolation
    (persistedId entity)
    (toJSON $ persistedValue entity)
    till

entityIsolationId :: Persisted e -> Text
entityIsolationId entity = 
  entityType (undefined :: Model.EntityIsolation) 
    ++ "-" ++ persistedId entity

-- | Restore the entity document under the same id and drop the isolation.
releaseIsolation :: (MonadAction m, Entity e)
  => Isolation e -- ^ The isolation returned by `isolateEntity`.
  -> m (Persisted e) -- ^ The restored entity.
releaseIsolation = 
  releaseIsolations . singleton >=> maybe fail return . listToMaybe
  where
    fail = throwIO $ ResponseException "EZCouch.EntityIsolation.releaseIsolation"

releaseIsolations :: (MonadAction m, Entity e)
  => [Isolation e]
  -> m [Persisted e]
releaseIsolations isolations = do
  results <- createIdentifiedEntities $ map isolationIdentified isolations
  case sequence results of
    Left _ -> throwIO $ OperationException $ 
      "Could not recreate entities under following ids when releasing the isolation: " ++ show (map fst . lefts $ results)
    Right entities -> do
      deleteEntitiesByIdRevs $ map isolationIdRev isolations
      return entities

-- | Get rid of both the isolation and the entity. The entity won't get restored
-- by the sweeper daemon after.
deleteIsolation :: (MonadAction m, Entity e)
  => Isolation e
  -> m ()
deleteIsolation = deleteIsolations . singleton

deleteIsolations :: (MonadAction m, Entity e)
  => [Isolation e]
  -> m ()
deleteIsolations = 
  void . tryOperation . deleteEntitiesByIdRevs . map isolationIdRev

