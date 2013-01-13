{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.ReadAction where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Data.Generics
import EZCouch.Action
import EZCouch.Types
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Data.Aeson.Types as Aeson
import qualified Util.Logging as Logging

import Debug.Trace

trace' a = trace (show a) a
log lvl = Logging.log "read-action" lvl


readAction includeDocs docType (ReadOptions keys view desc limit skip) = do
  case keys of
    Nothing -> getAction path (docTypeQPs ++ includeDocsQPs ++ optionsQPs) ""
    Just keys' -> postAction path (includeDocsQPs ++ optionsQPs) (Encoding.keysBody keys')
  where
    optionsQPs
      = catMaybes [descQP, limitQP, skipQP]
      where
        descQP = if desc then Just CC.QPDescending else Nothing
        limitQP = CC.QPLimit <$> limit
        skipQP = if skip /= 0 then Just $ CC.QPSkip skip else Nothing
    includeDocsQPs = if includeDocs then [CC.QPIncludeDocs] else []
    docTypeQPs = [CC.QPStartKey (docType ++ "-"), CC.QPEndKey (docType ++ "_")]
    path 
      | Just view' <- view = ["_design", docType, "_view", view']
      | otherwise = ["_all_docs"]

