{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Isolation where

import Prelude ()
import ClassyPrelude
import qualified Data.Time as Time

import EZCouch.Time
import EZCouch.Types
import EZCouch.Action hiding (logM)
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.View
import EZCouch.Model.Isolation as Isolation

import qualified Util.Logging as Logging

logM lvl = Logging.logM lvl "EZCouch.Isolation"

-- | Protect an action from being executed on multiple clients. Can be used to create transactions in a preemptive manner, i.e. instead of performing some actions and rolling back on transaction validation failure it does validation based on the provided identifier prior to actually executing the transaction. This function however does not provide you with atomicity guarantees (<http://en.wikipedia.org/wiki/Atomicity_(database_systems)>), as it does not rollback in case of client-interrupt - it's up to your algorithms to handle those cases.
inIsolation :: MonadAction m 
  => Int -- ^ A timeout in seconds. If after reaching it a conflicting isolation marker still exists in the db, it gets considered to be zombie (probably caused by a client interruption). The marker gets deleted and the current action gets executed.
  -> Text -- ^ A unique isolation identifier. It's a common practice to provide a 'persistedId' of the primary entity involved in the transaction, which is supposed to uniquely identify it.
  -> m a -- ^ The action to protect. Nothing of it will be executed if an isolation with the same id is already running.
  -> m (Maybe a) -- ^ Either the action's result or `Nothing` if it didn't get executed.
inIsolation timeout id action = do
  time <- readTime 
  result <- (try $ createEntityWithId id' $ Isolation time)
  case result of
    Left (OperationException _) -> do
      isolation <- readEntity ViewById (KeysSelectionList [id']) 0 False
      case isolation of
        Just isolation -> do
          if (Isolation.since . persistedValue) isolation < Time.addUTCTime (negate $ fromIntegral timeout) time
            then do 
              logM 0 $ "Deleting outdated isolation: " ++ id'
              tryToDelete isolation
              inIsolation timeout id action
            else do
              logM 0 $ "Skipping a busy isolation: " ++ id'
              return Nothing
        Nothing -> do
          logM 0 $ "Skipping a finished isolation: " ++ id'
          return Nothing
    Left e -> throwIO e
    Right isolation -> do
      logM 0 $ "Performing an isolation: " ++ id'
      finally (Just <$> action) (deleteEntity isolation)
  where 
    id' = "EZCouchIsolation-" ++ id

tryToDelete doc = (const True <$> deleteEntity doc) `catch` \e -> case e of
  OperationException _ -> return False
  _ -> throwIO e
