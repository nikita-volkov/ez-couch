{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch (
  -- * Execution Monad
  MonadAction(..),
  run,
  runWithManager,
  -- * CRUD Monadic Functions
  -- | All monadic functions are split into `CRUD` categories. The functions with a 'Multiple' suffix should be used for performing multiple operations at once.
  
  -- ** Creating Monadic Functions
  create,
  createMultiple,
  createWithId,
  createMultipleWithIds,
  -- ** Reading Monadic Functions
  readMultiple,
  readExists,
  readIds,
  readKeys,
  readCount,
  -- ** Updating Monadic Functions
  update,
  updateMultiple,
  -- ** Deleting Monadic Functions
  delete,
  deleteMultiple,
  -- * Working with Design Documents
  Design(..),
  createOrUpdateDesign,
  readDesign,
  -- * Types
  Persisted(..),
  EZCouchException(..),
  ReadOptions(..),
  readOptions,
  ConnectionSettings(..),
  defaultPort

) where

import EZCouch.Action
import EZCouch.Types as Types
import EZCouch.ReadAction as ReadAction
import EZCouch.BulkOperationsAction as BulkOperationsAction
import EZCouch.Design as Design
