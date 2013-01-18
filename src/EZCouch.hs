{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch (
  -- * Execution Monad
  MonadAction(..),
  run,
  runWithManager,
  -- ** CRUD Monadic Functions for Working with Records
  -- | All monadic functions are split into `CRUD` categories. The functions with a 'Multiple' suffix should be used for performing multiple operations at once.
  
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
  -- * Working with Design Documents
  Design(..),
  View(..),
  createOrUpdateDesign,
  readDesign,
  -- * Types
  Persisted(..),
  EZCouchException(..),
  ReadOptions(..),
  readOptions,
  ConnectionSettings(..),
  defaultPort,
  -- * Classes which records should implement
  Doc(..),
  -- ** Aeson re-exports
  ToJSON(..),
  FromJSON(..)
) where

import EZCouch.Action
import EZCouch.Types as Types
import EZCouch.ReadAction as ReadAction
import EZCouch.BulkOperationsAction as BulkOperationsAction
import EZCouch.Design as Design
import EZCouch.Doc as Design
import Data.Aeson as Aeson