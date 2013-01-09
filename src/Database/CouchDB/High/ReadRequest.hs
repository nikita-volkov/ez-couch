{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.CouchDB.High.ReadRequest where

import Prelude ()
import BasicPrelude hiding (log)

import Data.Generics
import qualified Data.Aeson.Types as Aeson
import qualified Database.CouchDB.Conduit as DB
import qualified Database.CouchDB.Conduit.View as DB
import qualified Database.CouchDB.Conduit.View.Query as DB
import qualified Database.CouchDB.Conduit.Implicit as DB
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Database.CouchDB.High.Types
import qualified Database.CouchDB.High.Request as Request
import qualified Database.CouchDB.High.Parsing as Parsing
import qualified Database.CouchDB.High.Encoding as Encoding

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Util.Logging as Logging

import Debug.Trace

trace' a = trace (Text.unpack $ show a) a

log lvl = Logging.log "read-request" lvl

readAll 
  :: (DB.MonadCouch m, Data a) 
  => DB -> ReadOptions -> m [Persisted a]
readAll db options
  = result
  where
    result = perform0 True Parsing.rowToPersisted mode db keys options
      where
        keys = Nothing :: (Maybe [Text])
        mode = DocTypeReadMode docType 
        docType = Encoding.docType $ resType result
        resType :: m [Persisted a] -> a
        resType _ = undefined

readIdsByType
  :: (DB.MonadCouch m) 
  => DB -> Text -> m [Text]
readIdsByType db docType
  = fmap (map fst . filter snd) 
    $ perform0 False Parsing.rowToBool mode db keys readOptions
  where
    keys = Nothing :: (Maybe [Text])
    mode = DocTypeReadMode docType 

readCountByType
  :: (DB.MonadCouch m) 
  => DB -> Text -> m Int
readCountByType db docType = fmap length $ readIdsByType db docType

readByIds 
  :: (DB.MonadCouch m, Data a) 
  => DB -> ReadOptions -> [Text] -> m [(Text, Maybe (Persisted a))]
readByIds db options ids
  = perform0 True Parsing.rowToMaybePersistedByKey AllReadMode db keys options
  where
    keys = Just ids

readExistingByIds 
  :: (DB.MonadCouch m, Data a) 
  => DB -> ReadOptions -> [Text] -> m [Persisted a]
readExistingByIds db options ids
  = fmap (catMaybes . snd . unzip)
    $ readByIds db options ids

readExistsByIds 
  :: (DB.MonadCouch m) 
  => DB -> [Text] -> m [(Text, Bool)]
readExistsByIds db ids
  = performKeysExist db (AllReadMode) ids

readExistingIds
  :: (DB.MonadCouch m) 
  => DB -> [Text] -> m [Text]
readExistingIds db ids 
  = fmap (map fst . filter snd) $ readExistsByIds db ids

readCountExistingIds
  :: (DB.MonadCouch m) 
  => DB -> [Text] -> m Int
readCountExistingIds db ids 
  = fmap length $ readExistingIds db ids


readFromView 
  :: (DB.MonadCouch m, Data a) 
  => DB -> View -> ReadOptions -> m [Persisted a]
readFromView db view options = result
  where
    result = perform0 True Parsing.rowToPersisted mode db keys options
      where
        keys = Nothing :: (Maybe [Text])
        mode = DocTypeViewReadMode docType view
        docType = Encoding.docType $ resType result
        resType :: m [Persisted a] -> a
        resType _ = undefined

readKeysFromView 
  :: (DB.MonadCouch m, Data k) 
  => DB -> DesignAndView -> m [k]
readKeysFromView db (design, view) 
  = result
  where
    result
      = fmap (map fst . filter snd) 
        $ perform0 False Parsing.rowToBool (DesignViewReadMode design view) db keys readOptions
        where
          keys = t result
          t :: m [k] -> Maybe [k]
          t _ = Nothing

readCountFromView 
  :: (DB.MonadCouch m) 
  => DB -> DesignAndView -> m Int
readCountFromView db designAndView
  = fmap length 
    $ (readKeysFromView db designAndView :: (DB.MonadCouch m) => m [Text])

readByKeysFromView 
  :: (DB.MonadCouch m, Data k, Data a) 
  => DB -> View -> ReadOptions -> [k] -> m [(k, Maybe (Persisted a))]
readByKeysFromView db view options keys = result
  where
    result = perform0 True Parsing.rowToMaybePersistedByKey mode db keys' options
      where
        keys' = Just keys
        mode = DocTypeViewReadMode docType view
        docType = Encoding.docType $ resType result
        resType :: m [(b, Maybe (Persisted a))] -> a
        resType _ = undefined

readExistingByKeysFromView 
  :: (DB.MonadCouch m, Data k, Data a) 
  => DB -> View -> ReadOptions -> [k] -> m [Persisted a]
readExistingByKeysFromView db view options keys
  = fmap (catMaybes . snd . unzip)
    $ readByKeysFromView db view options keys

readExistsByKeysFromView 
  :: (DB.MonadCouch m, Data k) 
  => DB -> DesignAndView -> [k] -> m [(k, Bool)]
readExistsByKeysFromView db (design, view) keys
  = performKeysExist db (DesignViewReadMode design view) keys

readExistingKeysFromView
  :: (DB.MonadCouch m, Data k) 
  => DB -> DesignAndView -> [k] -> m [k]
readExistingKeysFromView db designAndView keys
  = fmap (map fst . filter snd)
    $ readExistsByKeysFromView db designAndView keys

readCountByKeysFromView
  :: (DB.MonadCouch m, Data k) 
  => DB -> DesignAndView -> [k] -> m Int
readCountByKeysFromView db designAndView keys
  = fmap (length) 
    $ readExistingKeysFromView db designAndView keys


performKeysExist db mode keys
  = perform0 False Parsing.rowToBool mode db (Just keys) readOptions 

perform0
  :: (DB.MonadCouch m, Data b)
  => Bool -> (Aeson.Value -> a) -> ReadMode -> DB -> Maybe [b] -> ReadOptions -> m [a]
perform0 includeDocs parseRow mode db keys options = do
  src <- case keys of
    Nothing -> Request.getRead path (docTypeQPs ++ includeDocsQPs ++ optionsQPs) ""
    Just keys -> Request.postRead path (includeDocsQPs ++ optionsQPs) body
  src Conduit.$= Conduit.map parseRow Conduit.$$ Conduit.consume
  where
    path = DB.mkPath . map Text.encodeUtf8 $ db : readModePath mode
    optionsQPs = readOptionsQPs options
    includeDocsQPs = if includeDocs then [DB.QPIncludeDocs] else []
    docTypeQPs 
      | Just docType <- readModeDocType mode
        = [DB.QPStartKey (docType ++ "-"), DB.QPEndKey (docType ++ "_")]
      | otherwise = []
    body = Encoding.keysBody keys


data ReadOptions
  = ReadOptions {
      readOptionsDescending :: Bool,
      readOptionsLimit :: Maybe Int,
      readOptionsSkip :: Int
    }

readOptions = ReadOptions False Nothing 0

readOptionsQPs (ReadOptions desc limit skip)
  = catMaybes [desc', limit', skip']
  where
    desc' = if desc then Just DB.QPDescending else Nothing
    limit' = fmap DB.QPLimit limit
    skip' = if skip /= 0 then Just $ DB.QPSkip skip else Nothing



data ReadMode
  = DocTypeReadMode Text
  | DocTypeViewReadMode Text Text
  | DesignViewReadMode Text Text
  | AllReadMode

readModeDocType (DocTypeReadMode t) = Just t
readModeDocType (DocTypeViewReadMode t _) = Just t
readModeDocType _ = Nothing

readModePath mode = case mode of
  AllReadMode -> ["_all_docs"]
  DocTypeReadMode _ -> readModePath AllReadMode
  DocTypeViewReadMode t view -> ["_design", t, "_view", view]
  DesignViewReadMode design view -> ["_design", design, "_view", view]
