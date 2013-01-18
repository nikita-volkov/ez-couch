{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.ReadAction where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Monad.Trans.Resource
import EZCouch.Action
import EZCouch.Doc
import EZCouch.Types
import qualified EZCouch.Parsing as Parsing
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import Data.Aeson.Types

readAction
  :: (MonadAction m, Doc a, ToJSON k)
  => Bool
  -> ReadOptions a k
  -> m (ResumableSource m ByteString)
readAction includeDocs ro@(ReadOptions keys view desc limit skip) = case keys of
  Nothing -> getAction path (docTypeQPs ++ includeDocsQPs ++ optionsQPs) ""
  Just keys' -> postAction path (includeDocsQPs ++ optionsQPs) (Encoding.keysBody keys')
  where
    docType' = docType $ (undefined :: ReadOptions a k -> a) ro
    optionsQPs = catMaybes [descQP, limitQP, skipQP]
      where
        descQP = if desc then Just CC.QPDescending else Nothing
        limitQP = CC.QPLimit <$> limit
        skipQP = if skip /= 0 then Just $ CC.QPSkip skip else Nothing
    includeDocsQPs = if includeDocs then [CC.QPIncludeDocs] else []
    docTypeQPs = [CC.QPStartKey (docType' ++ "-"), CC.QPEndKey (docType' ++ "_")]
    path 
      | Just view' <- view = ["_design", docType', "_view", view']
      | otherwise = ["_all_docs"]
    descQP = if desc then Just CC.QPDescending else Nothing
    limitQP = CC.QPLimit <$> limit
    skipQP = if skip /= 0 then Just $ CC.QPSkip skip else Nothing


readMultiple :: (MonadAction m, Doc a, ToJSON k) => ReadOptions a k -> m [Persisted a]
readMultiple options 
  = readAction True options 
    >>= Parsing.parse Parsing.multipleRowsSink1 Parsing.persistedRowParser

readExists :: (MonadAction m, Doc a, ToJSON k, FromJSON k) => ReadOptions a k -> m [(k, Bool)]
readExists options
  = readAction False options
    >>= Parsing.parse Parsing.multipleRowsSink1 Parsing.keyExistsRowParser
    
readIds :: (MonadAction m, Doc a) => ReadOptions a Text -> m [Text]
readIds = readKeys

-- TODO: Test on returning ids for non-view queries
readKeys :: (MonadAction m, Doc a, ToJSON k, FromJSON k) => ReadOptions a k -> m [k]
readKeys = fmap (map fst . filter snd) . readExists

readCount :: (MonadAction m, Doc a, ToJSON k, FromJSON k) => ReadOptions a k -> m Int
readCount = fmap length . readKeys
