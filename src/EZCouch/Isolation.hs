{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Isolation where

import Prelude ()
import ClassyPrelude hiding (delete)

import EZCouch.Time
import EZCouch.Types
import EZCouch.Action
import EZCouch.BulkOperationsAction
import EZCouch.Model.Isolation as Isolation

-- | Protect an action from being executed on multiple clients. Can be used to create transactions in a preemptive manner, i.e. instead of performing some actions and rolling back on transaction validation failure, do the transaction validation based on the provided identifier prior to actually executing the transaction. This function however does not protect from unfinished transactions.
isolate :: MonadAction m 
  => Text -- ^ A unique isolation identifier. It's a common practice to provide a 'persistedId' of the primary entity involved in the transaction, which is supposed to uniquely identify it.
  -> m a -- ^ The action to protect. Nothing of it will be executed if an isolation with the same id is already running.
  -> m (Maybe a) -- ^ Either the action's result or `Nothing` if it didn't perform.
isolate id action = do
  time <- readTime
  result <- (try $ createWithId id' $ Isolation time)
  case result of
    Left (OperationException _) -> return Nothing
    Left e -> throwIO e
    Right isolation -> do
      result <- action
      delete isolation
      return $ Just result
  where 
    id' = "EZCouchIsolation-" ++ id

