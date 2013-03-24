{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, GADTs #-}
module EZCouch.ReadAction where

import Prelude ()
import ClassyPrelude.Conduit
import EZCouch.Action
import EZCouch.Entity
import EZCouch.Types
import EZCouch.Parsing
import EZCouch.View
import EZCouch.Logging
import EZCouch.Crash
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified System.Random as Random
import qualified EZCouch.Base62 as Base62
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import Data.Aeson.Types


data KeysSelection k
  = KeysSelectionAll
  | KeysSelectionRange k k
  | KeysSelectionRangeStart k
  | KeysSelectionRangeEnd k
  | KeysSelectionList [k]
  deriving (Show, Eq)


readAction :: (MonadAction m, Entity a, ToJSON k)
  => Bool -- ^ Include docs
  -> Int -- ^ Skip
  -> Maybe Int -- ^ Limit
  -> Bool -- ^ Descending
  -> KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m Value -- ^ An unparsed response body JSON
readAction includeDocs skip limit desc mode view = do
  result <- action path qps body 
  case result of
    ResponseNotFound -> do
      logLn 2 $ "View " 
        ++ fromMaybe undefined (viewGeneratedName view) 
        ++ " does not exist. Generating."
      createOrUpdateView view 
      action path qps body >>= \r -> case r of
        ResponseNotFound -> crash "readAction keeps getting a ResponseNotFound"
        ResponseOk json -> return json
    ResponseOk json -> return json
  where
    action = case mode of
      KeysSelectionList {} -> postAction
      _ -> getAction
    path = viewPath view
    qps = catMaybes [
        includeDocsQP includeDocs,
        startKeyQP view mode,
        endKeyQP view mode,
        descQP desc,
        limitQP limit,
        skipQP skip
      ]
    body = case mode of 
      KeysSelectionList keys -> Encoding.keysBody keys
      _ -> ""


startKeyQP _ (KeysSelectionRange start end) = Just $ CC.QPStartKey start
startKeyQP _ (KeysSelectionRangeStart start) = Just $ CC.QPStartKey start
startKeyQP _ (KeysSelectionList {}) = Nothing
startKeyQP view@ViewById _ = Just $ CC.QPStartKey $ viewDocType view ++ "-"
startKeyQP _ _ = Nothing

endKeyQP _ (KeysSelectionRange start end) = Just $ CC.QPEndKey end
endKeyQP _ (KeysSelectionRangeEnd end) = Just $ CC.QPEndKey end
endKeyQP _ (KeysSelectionList {}) = Nothing
endKeyQP view@ViewById _ = Just $ CC.QPEndKey $ viewDocType view ++ "."
endKeyQP _ _ = Nothing

limitQP limit = CC.QPLimit <$> limit

skipQP skip = if skip /= 0 then Just $ CC.QPSkip skip else Nothing

descQP desc = if desc then Just CC.QPDescending else Nothing

includeDocsQP True = Just CC.QPIncludeDocs
includeDocsQP False = Nothing


readKeys :: (MonadAction m, Entity a, ToJSON k, FromJSON k) 
  => KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m [k] 
readKeys mode view = fmap (map fst . filter snd) $ readKeysExist mode view

readCount :: (MonadAction m, Entity a, ToJSON k, FromJSON k)
  => KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m Int
readCount mode view = fmap length $ readKeys mode view

readKeysExist :: (MonadAction m, Entity a, ToJSON k, FromJSON k) 
  => KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m [(k, Bool)] 
  -- ^ An associative list of `Bool` values by keys designating the existance of appropriate entities
readKeysExist mode view =
  readAction False 0 Nothing False mode view
    >>= runParser (rowsParser1 >=> mapM keyExistsParser . toList) 

readEntities :: (MonadAction m, Entity a, ToJSON k)
  => Int -- ^ Skip
  -> Maybe Int -- ^ Limit
  -> Bool -- ^ Descending
  -> KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m [Persisted a]
readEntities skip limit desc mode view =
  readAction True skip limit desc mode view
    >>= runParser (rowsParser1 >=> mapM persistedParser . toList) 
    >>= return . catMaybes

readEntity :: (MonadAction m, Entity a, ToJSON k)
  => Int -- ^ Skip
  -> Bool -- ^ Descending
  -> KeysSelection k -- ^ Keys selection mode
  -> View a k -- ^ View
  -> m (Maybe (Persisted a))
readEntity skip desc mode view = 
  listToMaybe <$> readEntities skip (Just 1) desc mode view

readRandomEntities :: (MonadAction m, Entity a) 
  => Maybe Int -- ^ Limit
  -> m [Persisted a]
readRandomEntities limit = do
  startKey :: Double <- liftIO $ Random.randomRIO (0.0, 1.0)
  readEntities 
    0
    limit
    False
    (KeysSelectionRangeStart startKey)
    (ViewByKeys1 ViewKeyFloatRevHash) 


-- * Versions with defaults:
readKeys' = readKeys KeysSelectionAll
readCount' = readCount KeysSelectionAll
readKeysExist' = readKeysExist KeysSelectionAll
readEntities' = readEntities 0
readEntities'' = readEntities 0 Nothing
readEntities''' = readEntities 0 Nothing False
readEntities'''' = readEntities 0 Nothing False KeysSelectionAll
readEntities''''' = readEntities 0 Nothing False KeysSelectionAll ViewById
readEntity' = readEntity 0
readEntity'' = readEntity 0 False
readEntity''' = readEntity 0 False KeysSelectionAll
readEntity'''' = readEntity 0 False KeysSelectionAll ViewById
