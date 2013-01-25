{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Isolation where

import Prelude ()
import ClassyPrelude hiding (delete, log)
import qualified Data.Time as Time

import EZCouch.Time
import EZCouch.Types
import EZCouch.Action hiding (log)
import EZCouch.ReadAction
import EZCouch.BulkOperationsAction
import EZCouch.Model.Isolation as Isolation

import qualified Util.Logging as Logging

log lvl = Logging.log "EZCouch.Isolation" lvl

-- | Protect an action from being executed on multiple clients. Can be used to create transactions in a preemptive manner, i.e. instead of performing some actions and rolling back on transaction validation failure, do validation based on the provided identifier prior to actually executing the transaction. This function however does not provide you with guarantees that the action will either be executed in whole or not executed at all, as it does not rollback in case of client-interrupt - it's up to your algorithms to handle those cases.
isolate :: MonadAction m 
  => Int -- ^ A timeout in seconds. If after reaching it a conflicting isolation marker still exists in the db, it gets considered to be zombie (probably caused by a client interruption). The marker gets deleted and the current action gets executed.
  -> Text -- ^ A unique isolation identifier. It's a common practice to provide a 'persistedId' of the primary entity involved in the transaction, which is supposed to uniquely identify it.
  -> m a -- ^ The action to protect. Nothing of it will be executed if an isolation with the same id is already running.
  -> m (Maybe a) -- ^ Either the action's result or `Nothing` if it didn't get executed.
isolate timeout id action = do
  time <- readTime 
  result <- (try $ createWithId id' $ Isolation time)
  case result of
    Left (OperationException _) -> do
      isolation <- readOne $ readOptions { readOptionsKeys = Just [id'] }
      case isolation of
        Just isolation -> do
          if (Isolation.since . persistedValue) isolation < Time.addUTCTime (negate $ fromIntegral timeout) time
            then do 
              log 0 $ "Deleting outdated isolation: " ++ id'
              tryToDelete isolation
              isolate timeout id action
            else do
              log 0 $ "Skipping a busy isolation: " ++ id'
              return Nothing
        Nothing -> do
          log 0 $ "Skipping a finished isolation: " ++ id'
          return Nothing
    Left e -> throwIO e
    Right isolation -> do
      log 0 $ "Performing an isolation: " ++ id'
      finally (Just <$> action) (delete isolation)
  where 
    id' = "EZCouchIsolation-" ++ id

tryToDelete doc = (const True <$> delete doc) `catch` \e -> case e of
  OperationException _ -> return False
  _ -> throwIO e
