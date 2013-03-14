{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
-- | EZCouch is a library which takes a mission of bringing the topmost level of abstraction for working with CouchDB in Haskell. It abstracts away from loose concepts of this database and brings a strict static API over standard ADTs. 
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
  Path(..),

  -- * Transactions
  -- | CouchDB doesn't provide a way to do traditional locking-based transactions, as it applies an Optimistic Concurrency Control strategy (<http://en.wikipedia.org/wiki/Optimistic_concurrency_control>). EZCouch approaches the issue by providing a way to easily isolate entities from being accessed by concurrent clients, which you can use to build all kinds of transactions upon.
  isolateEntity,
  isolateEntities,
  releaseIsolation,
  releaseIsolations,
  deleteIsolation,
  deleteIsolations,
  Isolation,
  isolationEntity,
  -- * Types
  Persisted(..),
  persistedIdHashPart,

  -- * Helpers
  tryOperation,

  -- * Execution Monad
  MonadAction,
  Environment,
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

import Prelude ()
import ClassyPrelude

import EZCouch.Action
import EZCouch.Types
import EZCouch.ReadAction
import EZCouch.WriteAction
import EZCouch.View
import EZCouch.Entity
import EZCouch.Time
import EZCouch.Try
import EZCouch.EntityIsolation
import qualified EZCouch.Sweeper as Sweeper
import Data.Aeson

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Network.HTTP.Conduit as HTTP


runWithManager manager settings action = 
  flip runReaderT (settings, manager, fromIntegral 0) $ do
    timeDeviation <- getTimeDeviation
    withTimeDeviation timeDeviation $ runResourceT $ do
      resourceForkIO $ lift $ Sweeper.runSweeper
      lift $ action

run settings action = HTTP.withManager $ \manager -> 
  runWithManager manager settings action
