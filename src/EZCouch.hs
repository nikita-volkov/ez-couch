{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch (module Types, module ReadAction, module BulkOperationAction, run, runWithManager) where

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



