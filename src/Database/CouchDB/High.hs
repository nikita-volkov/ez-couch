{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.CouchDB.High (runWithManager, runCouch, module UpdateRequest, module ReadRequest, module Types, MonadCouch(..), Path(..), def, CouchConnection(..)) where

import Prelude ()
import BasicPrelude

import Data.Generics

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Database.CouchDB.Conduit as Conduit

import Database.CouchDB.High.UpdateRequest as UpdateRequest
import Database.CouchDB.High.Types as Types
import Database.CouchDB.High.ReadRequest as ReadRequest

runWithManager mgr cx
  = withCouchConnection mgr cx . runReaderT . runResourceT . lift

