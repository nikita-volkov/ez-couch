{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Design where

import Prelude ()
import ClassyPrelude
import Data.Generics

import EZCouch.ReadAction
import EZCouch.Action
import EZCouch.BulkOperationsAction
import EZCouch.Types
import EZCouch.Encoding
import EZCouch.Parsing


data Design a 
  = Design {
      views :: Map Text View  
    }
  deriving (Show, Eq, Data, Typeable)

data View 
  = View {
      map :: Text,
      reduce :: Maybe Text
    }
  deriving (Show, Eq, Data, Typeable)

readDesign :: (MonadAction m, Data a) => m (Maybe (Persisted (Design a)))
readDesign = result
  where
    result 
      = getAction ["_design", designName] [] "" 
        >>= parseSingleRow errorPersistedParser 
        >>= return . either (const Nothing) Just
      where
        designName = docType $ (undefined :: m (Maybe (Persisted (Design a))) -> a) result

-- readView :: (MonadAction m, Data a) => ViewId a -> m (Either (Text, Text) (Persisted (View a)))
-- readView viewId = getAction ["_design", viewIdDesign viewId] [] "" >>=
--   parseSingleRow singleRowSink errorPersistedParser

