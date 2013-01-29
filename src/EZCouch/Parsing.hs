{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Parsing where

import Prelude ()
import ClassyPrelude.Conduit
import qualified Data.Text.Lazy as Text
import EZCouch.Types
import Data.Aeson as Aeson 
import qualified Data.Conduit.Util as Conduit
import qualified Data.Conduit.Attoparsec as Atto
import qualified Data.Attoparsec as Atto

import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Fusion.Stream as Stream


parse rowsSink parser response = do
  rows <- response $$+- rowsSink
  result <- rows $= map parser $$ consume
  either (monadThrow . ParsingException) return $ sequence result

parseSingleRow parser response = do
  row <- response $$+- singleRowSink
  either (monadThrow . ParsingException) return $ parser row

singleRowSink :: MonadResource m => Sink ByteString m Aeson.Value
singleRowSink = Atto.sinkParser (Aeson.json Atto.<?> "Invalid JSON")

multipleRowsSink1 :: MonadResource m => Sink ByteString m (Source m Aeson.Value)
multipleRowsSink1 = do 
  o <- Atto.sinkParser (Aeson.json Atto.<?> "Invalid JSON")
  rows <- case o of
    Aeson.Object raw' -> case lookup "rows" raw' of
      Just (Aeson.Array r) -> return r
      _ -> return GVector.empty
    _ -> monadThrow $ ParsingException "Not an Object"
  return $ vectorSource rows

multipleRowsSink2 :: MonadResource m => Sink ByteString m (Source m Aeson.Value)
multipleRowsSink2 = do 
  Atto.sinkParser (Aeson.json Atto.<?> "Invalid JSON") >>= \r -> case r of
    Aeson.Array rows -> return $ vectorSource rows 
    _ -> monadThrow $ ParsingException "Not an Array"

vectorSource :: (Monad m, GVector.Vector v a) => v a -> Source m a
vectorSource vec = Conduit.sourceState (GVector.stream vec) f
  where f stream | Stream.null stream = return Conduit.StateClosed
                 | otherwise = return $ Conduit.StateOpen 
                      (Stream.tail stream) (Stream.head stream)


type RowParser a = Aeson.Value -> Either Text a

idRevRowParser :: RowParser (Text, Maybe Text)
idRevRowParser o @ (Aeson.Object m) 
  | Just rev <- lookup "rev" m,
    Just id <- lookup "id" m
    = (,) <$> fromJSON' id <*> (Just <$> fromJSON' rev)
  | Just code <- lookup "error" m,
    Just reason <- lookup "reason" m,
    Just id <- lookup "id" m
    = (,) <$> fromJSON' id <*> pure Nothing
  | otherwise
    = Left $ unexpectedRowValueText o

keyExistsRowParser :: (FromJSON k) => RowParser (k, Bool)
keyExistsRowParser o @ (Aeson.Object m) 
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
    = Left $ unexpectedRowValueText o

persistedRowParser :: (FromJSON a) => RowParser (Maybe (Persisted a))
persistedRowParser o
  | Just (Aeson.Bool True) <- o .? "value" ?.? "deleted"
    = Right Nothing
  | Just id <- o .? "id", 
    Just doc <- o .? "doc",
    Just rev <- doc .? "_rev"
    = fmap Just $ Persisted <$> fromJSON' id <*> fromJSON' rev <*> fromJSON' doc
  | otherwise
    = Left $ unexpectedRowValueText o

errorPersistedParser :: (FromJSON a) => RowParser (Either (Text, Text) (Persisted a))
errorPersistedParser o @ (Aeson.Object m) 
  | Just id <- lookup "_id" m,
    Just rev <- lookup "_rev" m
    = fmap Right $ Persisted <$> fromJSON' id <*> fromJSON' rev <*> fromJSON' o
  | Just error <- lookup "error" m, Just reason <- lookup "reason" m
    = fmap Left $ (,) <$> fromJSON' error <*> fromJSON' reason 
  | otherwise
    = Left $ unexpectedRowValueText o

maybePersistedByKeyRowParser :: (FromJSON a, FromJSON k) => RowParser (k, Maybe (Persisted a))
maybePersistedByKeyRowParser o @ (Aeson.Object m) 
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
    = Left $ unexpectedRowValueText o


fromJSON' v = case fromJSON v of
  Aeson.Success z -> Right $ z
  Aeson.Error s -> Left $ fromString $ s

unexpectedRowValueText o
  = "Unexpected row value: " ++ (Text.toStrict . decodeUtf8 $ Aeson.encode o)

o .? k = pure o ?.? k
o ?.? k = o >>= objectKey k
objectKey k (Aeson.Object m) = lookup k m
objectKey _ _ = Nothing