{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.Try where

import Prelude ()
import ClassyPrelude.Conduit

import EZCouch.Action
import EZCouch.Types
import EZCouch.WriteAction

-- | Return `Nothing` if an action throws an `OperationException` or `Just` its result otherwise.
-- 
-- This is only useful for a modifying actions (Create, Update, Delete).
tryOperation :: (MonadAction m) => m a -> m (Maybe a)
tryOperation action = (Just <$> action) `catch` \e -> case e of
  OperationException _ -> return Nothing
  _ -> throwIO e