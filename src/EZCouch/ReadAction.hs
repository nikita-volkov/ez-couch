{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, GADTs #-}
module EZCouch.ReadAction where

import Prelude ()
import ClassyPrelude.Conduit
import EZCouch.Action
import EZCouch.Doc
import EZCouch.Types
import EZCouch.Parsing
import EZCouch.View
import qualified EZCouch.Encoding as Encoding
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified System.Random as Random
import qualified EZCouch.Base62 as Base62
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import Data.Aeson.Types


-- data KeysReadOption k = 
--   KeysReadOptionAll |
--   KeysReadOptionRange (Maybe k) (Maybe k) |
--   KeysReadOptionList [k]
--   deriving (Show, Eq)

-- data ReadOptions a k = 
--   ReadOptions {
--     readOptionsView :: View a k,
--     readOptionsKeys :: KeysReadOption k,
--     readOptionsDescending :: Bool,
--     readOptionsLimit :: Maybe Int,
--     readOptionsSkip :: Int
--   }
--   deriving (Show, Eq)
  
-- readOptions :: ReadOptions a Text
-- readOptions = ReadOptions ViewAll KeysReadOptionAll False Nothing 0
-- readOptions = ReadOptions Nothing Nothing Nothing ViewAll False Nothing 0


-- .. Selection mode
data KeysSelection k
  = KeysSelectionAll
  | KeysSelectionRange k k
  | KeysSelectionRangeStart k
  | KeysSelectionRangeEnd k
  | KeysSelectionList [k]
  deriving (Show, Eq)

-- data FetchingOption
--   = FetchingOptionLimit Int
--   | FetchingOptionSkip Int
--   | FetchingOptionDesc


-- -- | Results assortion and filtering.
-- data ReadMode k
--   = ReadModeAll
--       Int -- ^ Skip
--       Int -- ^ Limit
--       Bool -- ^ Descending
--   | ReadModeRange 
--       k -- ^ Start Key
--       k -- ^ End Key
--       Bool -- ^ Descending
--   | ReadModeRangeNoEnd 
--       k -- ^ Start Key
--       Int -- ^ Limit
--       Bool -- ^ Descending
--   | ReadModeRangeNoStart 
--       k -- ^ End Key
--       Int -- ^ Limit
--       Bool -- ^ Descending
--   | ReadModeKeys 
--       [k] -- ^ Keys
--       Bool -- ^ Descending
--   deriving (Show, Eq)


readAction :: (MonadAction m, Doc a, ToJSON k)
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> Int -- ^ Skip
  -> Maybe Int -- ^ Limit
  -> Bool -- ^ Descending
  -> Bool -- ^ Include docs
  -> m Value -- ^ An unparsed response body JSON
readAction view mode skip limit desc includeDocs = 
  action path qps body `catch` \e -> case e of
    -- OperationException {} -> do
    HTTP.StatusCodeException (HTTP.Status code _) _ 
      | code `elem` [404, 500] 
      -> do
        createOrUpdateView view 
        action path qps body
    _ -> throwIO e
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

    -- qps = case mode of
    --   ReadModeAll skip limit desc -> concat [
    --       pure $ CC.QPLimit limit,
    --       repack $ skipQP skip,
    --       repack $ descQP desc,
    --       docTypeQPs (viewDocType view)
    --     ]
    --   ReadModeRange start end desc -> concat [
    --       pure $ CC.QPStartKey start,
    --       pure $ CC.QPEndKey end,
    --       repack $ descQP desc
    --     ]
    --   ReadModeRangeNoEnd start limit desc -> concat [
    --       pure $ CC.QPStartKey start,
    --       pure $ CC.QPLimit limit,
    --       repack $ descQP desc
    --     ]

    -- docTypeQPs = case mode of
    --   KeysSelection {} -> []


startKeyQP _ (KeysSelectionRange start end) = Just $ CC.QPStartKey start
startKeyQP _ (KeysSelectionRangeStart start) = Just $ CC.QPStartKey start
startKeyQP _ (KeysSelectionList {}) = Nothing
startKeyQP view@ViewAll _ = Just $ CC.QPStartKey $ viewDocType view ++ "-"
startKeyQP _ _ = Nothing
-- startKeyQP view mode = case view of
--   ViewAll -> case mode of
--     KeysSelectionList {} -> Nothing
--     KeysSelectionRange start end -> Just $ CC.QPStartKey start
--     KeysSelectionRangeStart start -> Just $ CC.QPStartKey start
--     _ -> Just $ CC.QPStartKey $ viewDocType view ++ "-"
--   _ -> Nothing

endKeyQP _ (KeysSelectionRange start end) = Just $ CC.QPEndKey end
endKeyQP _ (KeysSelectionRangeEnd end) = Just $ CC.QPEndKey end
endKeyQP _ (KeysSelectionList {}) = Nothing
endKeyQP view@ViewAll _ = Just $ CC.QPEndKey $ viewDocType view ++ "."
endKeyQP _ _ = Nothing
-- endKeyQP view mode = case view of
--   ViewAll -> case mode of
--     KeysSelectionList {} -> Nothing
--     KeysSelectionRange start end -> Just $ CC.QPEndKey end
--     KeysSelectionRangeEnd end -> Just $ CC.QPEndKey end
--     _ -> Just $ CC.QPEndKey $ viewDocType view ++ "."
--   _ -> Nothing

limitQP limit = CC.QPLimit <$> limit

skipQP skip = if skip /= 0 then Just $ CC.QPSkip skip else Nothing

descQP desc = if desc then Just CC.QPDescending else Nothing

includeDocsQP True = Just CC.QPIncludeDocs
includeDocsQP False = Nothing


readKeys :: (MonadAction m, Doc a, ToJSON k, FromJSON k) 
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> m [k] 
readKeys view mode = fmap (map fst . filter snd) $ readKeysExist view mode

readCount :: (MonadAction m, Doc a, ToJSON k, FromJSON k)
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> m Int
readCount view mode = fmap length $ readKeys view mode

readKeysExist :: (MonadAction m, Doc a, ToJSON k, FromJSON k) 
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> m [(k, Bool)] 
  -- ^ An associative list of `Bool` values by keys designating the existance of appropriate entities
readKeysExist view mode =
  readAction view mode 0 Nothing False False
    >>= runParser (rowsParser1 >=> mapM keyExistsParser . toList) 

readEntities :: (MonadAction m, Doc a, ToJSON k)
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> Int -- ^ Skip
  -> Maybe Int -- ^ Limit
  -> Bool -- ^ Descending
  -> m [Persisted a]
readEntities view mode skip limit desc =
  readAction view mode skip limit desc True
    >>= runParser (rowsParser1 >=> mapM persistedParser . toList) 
    >>= return . catMaybes

readEntity :: (MonadAction m, Doc a, ToJSON k)
  => View a k -- ^ View
  -> KeysSelection k -- ^ Keys selection mode
  -> Int -- ^ Skip
  -> Bool -- ^ Descending
  -> m (Maybe (Persisted a))
readEntity view mode skip desc = 
  listToMaybe <$> readEntities view mode skip (Just 1) desc

readRandomEntities :: (MonadAction m, Doc a) 
  => Maybe Int -- ^ Limit
  -> m [Persisted a]
readRandomEntities limit = do
  startKey :: Double <- liftIO $ Random.randomRIO (0.0, 1.0)
  readEntities 
    (ViewKeys1 ViewKeyRandom) 
    (KeysSelectionRangeStart startKey)
    0
    limit
    False

