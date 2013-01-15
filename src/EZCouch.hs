{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch (module Types, module ReadAction, module BulkOperationAction, run, run') where

import Prelude ()
import BasicPrelude

import Data.Generics

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

import EZCouch.Action
import EZCouch.Types as Types
import EZCouch.ReadAction as ReadAction
import EZCouch.BulkOperationAction as BulkOperationAction
-- import EZCouch.Design as Design

import qualified Network.HTTP.Conduit as HTTP

-- run' settings action = HTTP.withManager $ 
--   \manager -> liftIO $ run action settings manager

run' settings action = do
  manager <- HTTP.newManager HTTP.def
  result <- run action settings manager
  -- HTTP.closeManager manager
  return result

