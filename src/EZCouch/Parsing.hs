{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts #-}
module EZCouch.Parsing where

import Prelude ()
import ClassyPrelude
import Control.Monad.Trans.Resource
import qualified Data.Text.Lazy as Text
import EZCouch.Types
import Data.Aeson as Aeson 

type Parser a = Aeson.Value -> Either Text a

runParser parser response = 
  either (throwIO . ParsingException) return $ parser response

rowsParser1 :: Parser (Vector Aeson.Value)
rowsParser1 json
  | Just (Aeson.Array rows) <- json .? "rows" = Right rows
  | otherwise = Left $ unexpectedJSONValue json

rowsParser2 :: Parser (Vector Aeson.Value)
rowsParser2 json
  | Aeson.Array rows <- json = Right rows
  | otherwise = Left $ unexpectedJSONValue json

idRevParser :: Parser (Text, Maybe Text)
idRevParser o @ (Aeson.Object m) 
  | Just rev <- lookup "rev" m,
    Just id <- lookup "id" m
    = (,) <$> fromJSON' id <*> (Just <$> fromJSON' rev)
  | Just code <- lookup "error" m,
    Just reason <- lookup "reason" m,
    Just id <- lookup "id" m
    = (,) <$> fromJSON' id <*> pure Nothing
  | otherwise
    = Left $ unexpectedJSONValue o

keyExistsParser :: (FromJSON k) => Parser (k, Bool)
keyExistsParser o @ (Aeson.Object m) 
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> pure False
  | Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> pure False
  | Just id <- lookup "id" m,
    Just _ <- lookup "value" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> pure True
  | otherwise
    = Left $ unexpectedJSONValue o

persistedParser :: (FromJSON a) => Parser (Maybe (Persisted a))
persistedParser o
  | Just (Aeson.Bool True) <- o .? "value" ?.? "deleted"
    = Right Nothing
  | Just id <- o .? "id", 
    Just doc <- o .? "doc",
    Just rev <- doc .? "_rev"
    = fmap Just $ Persisted <$> fromJSON' id <*> fromJSON' rev <*> fromJSON' doc
  | otherwise
    = Left $ unexpectedJSONValue o

errorPersistedParser :: (FromJSON a) => Parser (Either (Text, Text) (Persisted a))
errorPersistedParser o @ (Aeson.Object m) 
  | Just id <- lookup "_id" m,
    Just rev <- lookup "_rev" m
    = fmap Right $ Persisted <$> fromJSON' id <*> fromJSON' rev <*> fromJSON' o
  | Just error <- lookup "error" m, Just reason <- lookup "reason" m
    = fmap Left $ (,) <$> fromJSON' error <*> fromJSON' reason 
  | otherwise
    = Left $ unexpectedJSONValue o

maybePersistedByKeyParser :: (FromJSON a, FromJSON k) => Parser (k, Maybe (Persisted a))
maybePersistedByKeyParser o @ (Aeson.Object m) 
  -- deleted
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just rev <- lookup "rev" valueM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> pure Nothing
  -- found
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just doc <- lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- lookup "_rev" docM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> (Just <$> (Persisted <$> fromJSON' id <*> fromJSON' rev <*> fromJSON' doc))
  -- not found
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON' key <*> pure Nothing
  | otherwise
    = Left $ unexpectedJSONValue o


fromJSON' json = case fromJSON json of
  Aeson.Success z -> Right $ z
  Aeson.Error s -> Left $ "fromJSON failed with a message `" 
    ++ fromString s 
    ++ "` on the following value: " 
    ++ (Text.toStrict . decodeUtf8 $ Aeson.encode json) 

unexpectedJSONValue json = 
  "Unexpected JSON value: " ++ (Text.toStrict . decodeUtf8 $ Aeson.encode json)

o .? k = pure o ?.? k
o ?.? k = o >>= objectKey k
objectKey k (Aeson.Object m) = lookup k m
objectKey _ _ = Nothing