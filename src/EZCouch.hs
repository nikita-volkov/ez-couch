{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
-- | EZCouch is a library which takes a mission of bringing the topmost level of abstraction for working with CouchDB from Haskell. It abstracts away from loose concepts of this database and brings a strict static API over standard ADTs. 
module EZCouch (
  -- * CRUD Monadic Functions for Working with Records
  -- | All monadic functions are split into /CRUD/ categories. The functions with a /Multiple/ suffix are better alternatives for performing multiple operations at once.

  -- ** Creating 
  create,
  createMultiple,
  -- ** Reading 
  -- | All reading actions accept a `ReadOptions` parameter which specifies how filtering and ordering should go.
  readOne,
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
  
  -- * Server Time
  readTime,

  -- * Working with Views
  createOrUpdateView,  

  -- * Transactions
  -- | CouchDB doesn't provide a way to do traditional locking-based transactions, as it applies an Optimistic Concurrency Control strategy (<http://en.wikipedia.org/wiki/Optimistic_concurrency_control>). EZCouch approaches the issue by abstracting over it.
  inIsolation,

  -- * Types
  Persisted(..),
  EZCouchException(..),
  View(..),
  ReadOptions(..),
  readOptions,
  ConnectionSettings(..),
  defaultPort,

  -- * Helpers
  tryOperation,

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
import EZCouch.WriteAction
import EZCouch.View
import EZCouch.Doc
import EZCouch.Time
import EZCouch.Isolation
import EZCouch.Try
import Data.Aeson
