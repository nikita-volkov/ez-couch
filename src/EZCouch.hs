{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch (
  -- * CRUD Monadic Functions for Working with Records
  -- | All monadic functions are split into /CRUD/ categories. The functions with a /Multiple/ suffix are better alternatives for performing multiple operations at once.

  -- ** Creating 
  create,
  createMultiple,
  createWithId,
  createMultipleWithIds,
  -- ** Reading 
  readMultiple,
  readExists,
  readIds,
  readKeys,
  readCount,
  -- ** Updating 
  update,
  updateMultiple,
  -- ** Deleting 
  delete,
  deleteMultiple,
  
  -- * Working with Views
  createOrUpdateView,  

  -- * Types
  Persisted(..),
  EZCouchException(..),
  View(..),
  ReadOptions(..),
  readOptions,
  ConnectionSettings(..),
  defaultPort,

  -- * Execution Monad
  MonadAction(..),
  run,
  runWithManager,

  -- * Classes which records should implement
  Doc(..),
  -- ** Aeson re-exports
  ToJSON(..),
  FromJSON(..)
) where

import EZCouch.Action
import EZCouch.Types
import EZCouch.ReadAction
import EZCouch.BulkOperationsAction
import EZCouch.View
import EZCouch.Doc
import Data.Aeson