{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.CouchDB.High.UpdateRequest where

import Prelude ()
import BasicPrelude
import Control.Exception.Lifted 
import Data.Either
import Data.String (fromString)

import Database.CouchDB.High.Ids 
import Database.CouchDB.High.Request
import Database.CouchDB.High.ReadRequest as ReadRequest
import Database.CouchDB.High.Types
import qualified Database.CouchDB.High.BulkPostRequest as BulkPostRequest
import qualified Database.CouchDB.High.Encoding as Encoding
import qualified Database.CouchDB.High.Parsing as Parsing

import Data.Generics
import Data.Aeson.Types as Aeson hiding (toJSON, fromJSON)
import Data.Aeson.Types as StaticAeson 
import Data.Aeson.FixedGeneric as Aeson 
import Data.Aeson.Encode as StaticAeson
import qualified Data.HashMap.Lazy as HashMap
import Database.CouchDB.Conduit 
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString


create :: (MonadCouch m, Data a) => DB -> a -> m (Persisted a)
create db a = fmap head $ createMultiple db [a]

createMultiple_ :: (MonadCouch m, Data a) => DB -> [(Text, a)] -> m [Either (Text, a) (Persisted a)]
createMultiple_ db idsToVals = do
  results <- bulkPost db ops
  return $ map convertResult results
  where
    convertResult (Left id) = Left (id, valById id)
    convertResult (Right (id, rev)) = Right (Persisted id rev (valById id))
    ops = [Create id val | (id, val) <- idsToVals]
    valById id
      | Just val <- HashMap.lookup id cache = val
      | otherwise = throw $ ResponseException $ "Unexpected id: " ++ id
      where
        cache = HashMap.fromList idsToVals
    
createMultiple :: (MonadCouch m, Data a) => DB -> [a] -> m [Persisted a]
createMultiple db vals = perform vals 10
  where
    generateIdToVal val = do
      id <- fmap ((Encoding.docType val ++ "-") ++) $ fmap fromString generateId
      return (id, val)
    generateIdsToVals vals = mapM generateIdToVal vals

    perform vals attempts = do
      idsToVals <- liftIO $ generateIdsToVals vals
      results <- createMultiple_ db idsToVals
      let (failures, successes) = partitionEithers results
      if attempts > 0 || null failures 
        then do
          let vals' = [val | (_, val) <- failures]
          let attempts' = if null successes then attempts - 1 else attempts
          remaining <- if null failures then return [] else perform vals' attempts'
          return $ remaining ++ successes
        else
          throw $ OperationException $ "Couldn't create all documents"


update :: (MonadCouch m, Data a) => DB -> Persisted a -> m (Persisted a)
update db pd = updateMultiple db [pd] >>= return . head

updateMultiple :: (MonadCouch m, Data a) => DB -> [Persisted a] -> m [Persisted a]
updateMultiple db ps = do
  (failures, successes) <- fmap partitionEithers $ bulkPost db $ map toOp ps
  if null failures
    then return $ map toResult successes
    else throw $ OperationException $ "Couldn't update all documents"
  where
    toOp (Persisted id rev val) = Update id rev val
    toResult (id, rev)
      | Just val <- HashMap.lookup id cache = (Persisted id rev val)
      | otherwise = throw $ ResponseException $ "Unexpected id: " ++ id
      where
        cache = HashMap.fromList [(id, val) | (Persisted id _ val) <- ps]



delete :: (MonadCouch m, Data a) => DB -> Persisted a -> m ()
delete db pVal = deleteMultiple db [pVal]

deleteMultiple :: (MonadCouch m, Data a) => DB -> [Persisted a] -> m ()
deleteMultiple db pVals = do
  results <- bulkPost db $ map toOperation pVals
  let failures = lefts results
  if null failures
    then return ()
    else throw $ OperationException $ "Couldn't delete entities by following ids: " ++ show failures
  where
    toOperation (Persisted id rev val) = Delete id rev val




data BulkOperation a
  = Create Text a
  | Update Text Text a
  | Delete Text Text a

bulkPost 
  :: (MonadCouch m, Data a) 
  => DB -> [BulkOperation a] -> m [Either Text (Text, Text)]
bulkPost db ops = do
  results <- BulkPostRequest.perform db $ map encodeOperation ops
  return $ map decodeResult results
  where
    encodeOperation (Create id val)
      = BulkPostRequest.Create (StaticAeson.toJSON id) (Aeson.toJSON val)
    encodeOperation (Update id rev val)
      = BulkPostRequest.Update (StaticAeson.toJSON id) (StaticAeson.toJSON rev) (Aeson.toJSON val)
    encodeOperation (Delete id rev _)
      = BulkPostRequest.Delete (StaticAeson.toJSON id) (StaticAeson.toJSON rev)

    decodeResult (Left id) = Left $ Parsing.fromJSON id
    decodeResult (Right (id, rev)) = Right $ (Parsing.fromJSON id, Parsing.fromJSON rev)

