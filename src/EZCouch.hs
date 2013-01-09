{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EZCouch (runWithManager, runCouch, module UpdateRequest, module ReadRequest, module Types, MonadCouch(..), Path(..), def, CouchConnection(..)) where

import Prelude ()
import BasicPrelude

import Data.Generics

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Database.CouchDB.Conduit as Conduit

import EZCouch.UpdateRequest as UpdateRequest
import EZCouch.Types as Types
import EZCouch.ReadRequest as ReadRequest

runWithManager mgr cx
  = withCouchConnection mgr cx . runReaderT . runResourceT . lift

