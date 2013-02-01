{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.WriteAction where

import Prelude ()
import ClassyPrelude.Conduit
import Control.Monad.Trans.Resource
import EZCouch.Ids 
import EZCouch.Action
import EZCouch.Types
import EZCouch.Doc
import EZCouch.Parsing
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import Data.Aeson as Aeson

data WriteOperation a
  = Create Text a
  | Update Text Text a
  | Delete Text Text

writeOperationsAction :: (MonadAction m, Doc a) 
  => [WriteOperation a] 
  -> m [(Text, Maybe Text)]
  -- ^ Maybe rev by id. Nothing on failure.
writeOperationsAction ops =
  postAction path qps body >>= 
    runParser (rowsParser2 >=> mapM idRevParser . toList)
  where
    path = ["_bulk_docs"]
    qps = []
    body = writeOperationsBody ops

writeOperationsBody ops = Aeson.encode $ Aeson.object [("docs", Aeson.Array $ fromList $ map operationJSON ops)]

operationJSON (Create id a)
  = Encoding.insertPairs [("_id", toJSON id)] $ toJSON a
operationJSON (Update id rev a)
  = Encoding.insertPairs [("_id", toJSON id), ("_rev", toJSON rev)] $ toJSON a
operationJSON (Delete id rev)
  = Aeson.object [("_id", toJSON id), ("_rev", toJSON rev), ("_deleted", Aeson.Bool True)] 

deleteEntities :: (MonadAction m, Doc a) => [Persisted a] -> m ()
deleteEntities vals = do
  results <- writeOperationsAction $ map toOperation vals
  let failedIds = map fst $ filter (isNothing . snd) results
  if null failedIds
    then return ()
    else throwIO $ OperationException $ "Couldn't delete entities by following ids: " ++ show failedIds
  where
    toOperation :: Persisted a -> WriteOperation a
    toOperation (Persisted id rev val) = Delete id rev

deleteEntity :: (MonadAction m, Doc a) => Persisted a -> m ()
deleteEntity = deleteEntities . singleton

createEntitiesWithIds :: (MonadAction m, Doc a) 
  => [(Text, a)] 
  -> m [Either (Text, a) (Persisted a)]
createEntitiesWithIds idsToVals 
  = writeOperationsAction [Create id val | (id, val) <- idsToVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList idsToVals
    convertResult (id, Nothing) = fmap Left $ 
      (,) <$> pure id <*> lookupThrowing id valById
    convertResult (id, Just rev) = fmap Right $ 
      Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

createEntityWithId :: (MonadAction m, Doc a)
  => Text
  -> a
  -> m (Persisted a)
createEntityWithId id val = createEntitiesWithIds [(id, val)] 
  >>= return . join . fmap (either (const Nothing) Just) . listToMaybe 
  >>= maybe (throwIO $ OperationException "Failed to create entity") return

createEntities :: (MonadAction m, Doc a) => [a] -> m [Persisted a]
createEntities = retry 10 
  where
    generateIdToVal val = do
      id <- fmap ((docType val ++ "-") ++) $ fmap fromString generateId
      return (id, val)
    retry attempts vals = do    
      idsToVals <- liftIO $ mapM generateIdToVal vals
      results <- createEntitiesWithIds idsToVals
      let (failures, successes) = partitionEithers results
      if attempts > 0 || null failures 
        then do
          let vals' = [val | (_, val) <- failures]
          let attempts' = if null successes then attempts - 1 else attempts
          remaining <- if null failures then return [] else retry attempts' vals'
          return $ remaining ++ successes
        else
          throwIO $ OperationException $ "Failed to generate unique ids"

createEntity :: (MonadAction m, Doc a) => a -> m (Persisted a)
createEntity = return . singleton >=> createEntities >=> 
  maybe (throwIO $ OperationException "Failed to create entity") return . listToMaybe

updateEntities :: (MonadAction m, Doc a) => [Persisted a] -> m [Persisted a]
updateEntities pVals
  = writeOperationsAction [Update id rev val | Persisted id rev val <- pVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList [(id, val) | Persisted id _ val <- pVals]
    convertResult (id, Nothing) = throwIO $ OperationException $ "Couldn't updateEntity all documents"
    convertResult (id, Just rev) = Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

updateEntity :: (MonadAction m, Doc a) => Persisted a -> m (Persisted a)
updateEntity = return . singleton >=> updateEntities >=> 
  maybe (throwIO $ OperationException "Failed to update entity") return . listToMaybe
    
lookupThrowing id cache = case lookup id cache of
  Just val -> return val
  Nothing -> throwIO $ ParsingException $ "Unexpected id: " ++ show id
