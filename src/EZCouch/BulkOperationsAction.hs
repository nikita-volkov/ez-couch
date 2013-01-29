{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.BulkOperationsAction where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Monad.Trans.Resource
import EZCouch.Ids 
import EZCouch.Action
import EZCouch.Types
import EZCouch.Doc
import EZCouch.Parsing
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import Data.Aeson as Aeson

data BulkOperation a
  = Create Text a
  | Update Text Text a
  | Delete Text Text

bulkOperationsAction :: (MonadAction m, Doc a) 
  => [BulkOperation a] 
  -> m [(Text, Maybe Text)]
  -- ^ Maybe rev by id. Nothing on failure.
bulkOperationsAction ops =
  postAction path qps body >>= 
    runParser (rowsParser2 >=> mapM idRevParser . toList)
  where
    path = ["_bulk_docs"]
    qps = []
    body = bulkOperationsBody ops

bulkOperationsBody ops = Aeson.encode $ Aeson.object [("docs", Aeson.Array $ fromList $ map operationJSON ops)]

operationJSON (Create id a)
  = Encoding.insertPairs [("_id", toJSON id)] $ toJSON a
operationJSON (Update id rev a)
  = Encoding.insertPairs [("_id", toJSON id), ("_rev", toJSON rev)] $ toJSON a
operationJSON (Delete id rev)
  = Aeson.object [("_id", toJSON id), ("_rev", toJSON rev), ("_deleted", Aeson.Bool True)] 

deleteMultiple :: (MonadAction m, Doc a) => [Persisted a] -> m ()
deleteMultiple vals = do
  results <- bulkOperationsAction $ map toOperation vals
  let failedIds = map fst $ filter (isNothing . snd) results
  if null failedIds
    then return ()
    else throwIO $ OperationException $ "Couldn't delete entities by following ids: " ++ show failedIds
  where
    toOperation :: Persisted a -> BulkOperation a
    toOperation (Persisted id rev val) = Delete id rev

delete :: (MonadAction m, Doc a) => Persisted a -> m ()
delete = deleteMultiple . singleton

createMultipleWithIds :: (MonadAction m, Doc a) 
  => [(Text, a)] 
  -> m [Either (Text, a) (Persisted a)]
createMultipleWithIds idsToVals 
  = bulkOperationsAction [Create id val | (id, val) <- idsToVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList idsToVals
    convertResult (id, Nothing) = fmap Left $ 
      (,) <$> pure id <*> lookupThrowing id valById
    convertResult (id, Just rev) = fmap Right $ 
      Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

createWithId :: (MonadAction m, Doc a)
  => Text
  -> a
  -> m (Persisted a)
createWithId id val = createMultipleWithIds [(id, val)] 
  >>= return . join . fmap (either (const Nothing) Just) . listToMaybe 
  >>= maybe (throwIO $ OperationException "Failed to create entity") return

createMultiple :: (MonadAction m, Doc a) => [a] -> m [Persisted a]
createMultiple = retry 10 
  where
    generateIdToVal val = do
      id <- fmap ((docType val ++ "-") ++) $ fmap fromString generateId
      return (id, val)
    retry attempts vals = do    
      idsToVals <- liftIO $ mapM generateIdToVal vals
      results <- createMultipleWithIds idsToVals
      let (failures, successes) = partitionEithers results
      if attempts > 0 || null failures 
        then do
          let vals' = [val | (_, val) <- failures]
          let attempts' = if null successes then attempts - 1 else attempts
          remaining <- if null failures then return [] else retry attempts' vals'
          return $ remaining ++ successes
        else
          throwIO $ OperationException $ "Failed to generate unique ids"

create :: (MonadAction m, Doc a) => a -> m (Persisted a)
create = return . singleton >=> createMultiple >=> 
  maybe (throwIO $ OperationException "Failed to create entity") return . listToMaybe

updateMultiple :: (MonadAction m, Doc a) => [Persisted a] -> m [Persisted a]
updateMultiple pVals
  = bulkOperationsAction [Update id rev val | Persisted id rev val <- pVals]
      >>= mapM convertResult
  where
    valById = asMap $ fromList [(id, val) | Persisted id _ val <- pVals]
    convertResult (id, Nothing) = throwIO $ OperationException $ "Couldn't update all documents"
    convertResult (id, Just rev) = Persisted <$> pure id <*> pure rev <*> lookupThrowing id valById

update :: (MonadAction m, Doc a) => Persisted a -> m (Persisted a)
update = return . singleton >=> updateMultiple >=> 
  maybe (throwIO $ OperationException "Failed to update entity") return . listToMaybe
    
lookupThrowing id cache = case lookup id cache of
  Just val -> return val
  Nothing -> throwIO $ ParsingException $ "Unexpected id: " ++ show id
