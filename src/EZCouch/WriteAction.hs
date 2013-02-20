{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.WriteAction where

import Prelude ()
import ClassyPrelude.Conduit
import Control.Monad.Trans.Resource
import EZCouch.Ids 
import EZCouch.Action
import EZCouch.Types
import EZCouch.Entity
import EZCouch.Parsing
import EZCouch.Crash
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import Data.Aeson as Aeson

data WriteOperation a
  = Create Text a
  | Update Text Text a
  | Delete Text Text

writeOperationsAction :: (MonadAction m, ToJSON a) 
  => [WriteOperation a] 
  -> m [(Text, Maybe Text)]
  -- ^ Maybe rev by id. Nothing on failure.
writeOperationsAction ops =
  postAction path qps body >>= \r -> case r of
    ResponseNotFound -> crash $ "EZCouch.WriteAction.writeOperationsAction: unexpected Not Found response"
    ResponseOk json -> 
      runParser (rowsParser2 >=> mapM idRevParser . toList) json
  where
    path = ["_bulk_docs"]
    qps = []
    body = writeOperationsBody ops

writeOperationsBody ops = Aeson.encode $ Aeson.object [("docs", Aeson.Array $ fromList $ fmap operationJSON ops)]

operationJSON (Create id a)
  = Encoding.insertPairs [("_id", toJSON id)] $ toJSON a
operationJSON (Update id rev a)
  = Encoding.insertPairs [("_id", toJSON id), ("_rev", toJSON rev)] $ toJSON a
operationJSON (Delete id rev)
  = Aeson.object [("_id", toJSON id), ("_rev", toJSON rev), ("_deleted", Aeson.Bool True)] 

deleteEntitiesByIdRevs :: (MonadAction m, Entity a) => [IdRev a] -> m ()
deleteEntitiesByIdRevs idRevs = do
  results <- writeOperationsAction $ map toOperation idRevs
  let failedIds = fmap fst $ filter (isNothing . snd) results
  if null failedIds
    then return ()
    else throwIO $ OperationException $ "Couldn't delete entities by following ids: " ++ show failedIds
  where
    toOperation :: IdRev a -> WriteOperation a
    toOperation (IdRev id rev) = Delete id rev

deleteEntities :: (MonadAction m, Entity a) => [Persisted a] -> m ()
deleteEntities = deleteEntitiesByIdRevs . map persistedIdRev

deleteEntity :: (MonadAction m, Entity a) => Persisted a -> m ()
deleteEntity = deleteEntities . singleton

createIdentifiedEntities :: (MonadAction m, ToJSON a) 
  => [Identified a]
  -> m [Either (Identified a) (Persisted a)]
createIdentifiedEntities idsToVals 
  = writeOperationsAction [Create id val | (id, val) <- idsToVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList idsToVals
    convertResult (id, Nothing) = fmap Left $ 
      (,) <$> pure id <*> lookupThrowing id valById
    convertResult (id, Just rev) = fmap Right $ 
      Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

createIdentifiedEntity :: (MonadAction m, Entity a)
  => Identified a
  -> m (Persisted a)
createIdentifiedEntity = 
  createIdentifiedEntities . singleton 
    >=> return . join . fmap (either (const Nothing) Just) . listToMaybe 
    >=> maybe (throwIO $ OperationException "Failed to create entity") return

createEntities :: (MonadAction m, Entity a) => [a] -> m [Persisted a]
createEntities = retry 10 
  where
    generateIdToVal val = do
      id <- fmap ((entityType val ++ "-") ++) $ fmap fromString generateId
      return (id, val)
    retry attempts vals = do    
      idsToVals <- liftIO $ mapM generateIdToVal vals
      results <- createIdentifiedEntities idsToVals
      let (failures, successes) = partitionEithers results
      if attempts > 0 || null failures 
        then do
          let vals' = [val | (_, val) <- failures]
          let attempts' = if null successes then attempts - 1 else attempts
          remaining <- if null failures then return [] else retry attempts' vals'
          return $ remaining ++ successes
        else
          throwIO $ OperationException $ "Failed to generate unique ids"

createEntity :: (MonadAction m, Entity a) => a -> m (Persisted a)
createEntity = return . singleton >=> createEntities >=> 
  maybe (throwIO $ OperationException "Failed to create entity") return . listToMaybe

updateEntities :: (MonadAction m, Entity a) => [Persisted a] -> m [Persisted a]
updateEntities pVals
  = writeOperationsAction [Update id rev val | Persisted id rev val <- pVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList [(id, val) | Persisted id _ val <- pVals]
    convertResult (id, Nothing) = throwIO $ OperationException $ "Couldn't updateEntity all documents"
    convertResult (id, Just rev) = Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

updateEntity :: (MonadAction m, Entity a) => Persisted a -> m (Persisted a)
updateEntity = return . singleton >=> updateEntities >=> 
  maybe (throwIO $ OperationException "Failed to update entity") return . listToMaybe
    
lookupThrowing id cache = case lookup id cache of
  Just val -> return val
  Nothing -> throwIO $ ResponseException $ "Unexpected id: " ++ show id
