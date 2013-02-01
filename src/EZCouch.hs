{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
-- | EZCouch is a library which takes a mission of bringing the topmost level of abstraction for working with CouchDB from Haskell. It abstracts away from loose concepts of this database and brings a strict static API over standard ADTs. 
module EZCouch (
  -- * CRUD Monadic Functions for Working with Records
  -- | All monadic functions are split into /CRUD/ categories. The functions with a /Multiple/ suffix are better alternatives for performing multiple operations at once.

  -- ** Creating 
  createEntity,
  createEntities,
  -- ** Reading 
  readEntities,
  readRandomEntities,
  readEntity,
  readKeysExist,
  readKeys,
  readCount,
  KeysSelection(..),
  -- ** Updating 
  updateEntity,
  updateEntities,
  -- ** Deleting 
  deleteEntity,
  deleteEntities,
  
  -- * Server Time
  readTime,

  -- * Working with Views
  View(..),
  ViewKey(..),

  -- * Transactions
  -- | CouchDB doesn't provide a way to do traditional locking-based transactions, as it applies an Optimistic Concurrency Control strategy (<http://en.wikipedia.org/wiki/Optimistic_concurrency_control>). EZCouch approaches the issue by abstracting over it.
  inIsolation,

  -- * Types
  Persisted(..),

  -- * Helpers
  tryOperation,

  -- * Execution Monad
  MonadAction(..),
  run,
  runWithManager,
  ConnectionSettings(..),
  defaultPort,
  EZCouchException(..),

  -- * Classes which records should implement
  Entity(..),
  -- ** Aeson re-exports
  ToJSON(..),
  FromJSON(..)
) where

import EZCouch.Action
import EZCouch.Types
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.View
import EZCouch.Entity
import EZCouch.Time
import EZCouch.Isolation
import EZCouch.Try
import Data.Aeson
