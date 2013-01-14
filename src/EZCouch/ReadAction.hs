{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.ReadAction where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Monad.Trans.Resource
import Data.Generics
import EZCouch.Action
import EZCouch.Types
import qualified EZCouch.Parsing as Parsing
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Data.Aeson.Types as Aeson
import qualified Util.Logging as Logging

import Debug.Trace

trace' a = trace (show a) a
log lvl = Logging.log "read-action" lvl

readAction
  :: (Data k, Data a) 
  => Bool
  -> ReadOptions k
  -> Action a (ResumableSource (ResourceT IO) ByteString)
readAction includeDocs (ReadOptions keys view desc limit skip) = result
  where
    result = case keys of
      Nothing -> getAction path (docTypeQPs ++ includeDocsQPs ++ optionsQPs) ""
      Just keys' -> postAction path (includeDocsQPs ++ optionsQPs) (Encoding.keysBody keys')
      where
        docType = Encoding.docType $ actionEntityType result
        optionsQPs = catMaybes [descQP, limitQP, skipQP]
          where
            descQP = if desc then Just CC.QPDescending else Nothing
            limitQP = CC.QPLimit <$> limit
            skipQP = if skip /= 0 then Just $ CC.QPSkip skip else Nothing
        includeDocsQPs = if includeDocs then [CC.QPIncludeDocs] else []
        docTypeQPs = [CC.QPStartKey (docType ++ "-"), CC.QPEndKey (docType ++ "_")]
        path 
          | Just view' <- view = ["_design", docType, "_view", view']
          | otherwise = ["_all_docs"]


readEntities :: (Data a, Data k) => ReadOptions k -> Action a [Persisted a]
readEntities options 
  = readAction True options 
    >>= Parsing.parse Parsing.multipleRowsSink1 Parsing.persistedRowParser

readExists :: (Data a, Data k) => ReadOptions k -> Action a [(k, Bool)]
readExists options
  = readAction False options
    >>= Parsing.parse Parsing.multipleRowsSink1 Parsing.keyExistsRowParser
    
-- readIds :: ReadOptions -> Action [ByteString]

-- TODO: Should return ids for non-view queries
readKeys :: (Data a, Data k) => ReadOptions k -> Action a [k]
readKeys = fmap (mapMaybe f) . readExists
  where f (k, b) = if b then Just k else Nothing

readCount :: (Data a, Data k) => ReadOptions k -> Action a Int
readCount = fmap length . readKeys
