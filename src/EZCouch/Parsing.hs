{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Parsing where

import Prelude ()
import ClassyPrelude.Conduit
import Control.Exception.Lifted 
import Data.Text.Lazy (toStrict)
import Data.Generics
import EZCouch.Types
import qualified Data.Aeson as Aeson 
import qualified Data.Aeson.FixedGeneric as GAeson 
import qualified Data.Conduit.Util as Conduit
import qualified Data.Conduit.Attoparsec as Atto
import qualified Data.Attoparsec as Atto

import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Fusion.Stream as Stream


oneRowSink :: MonadResource m => Sink ByteString m Aeson.Value
oneRowSink = Atto.sinkParser (Aeson.json Atto.<?> "json object")

readRowsSink :: MonadResource m => Sink ByteString m (Source m Aeson.Value)
readRowsSink = do 
  o <- Atto.sinkParser (Aeson.json Atto.<?> "Invalid JSON")
  rows <- case o of
    Aeson.Object raw' -> case lookup "rows" raw' of
      Just (Aeson.Array r) -> return r
      _ -> return GVector.empty
    _ -> monadThrow $ ParsingException "Not an Object"
  return $ vectorSource rows

updateRowsSink :: MonadResource m => Sink ByteString m (Source m Aeson.Value)
updateRowsSink = do 
  Atto.sinkParser (Aeson.json Atto.<?> "Invalid JSON") >>= \r -> case r of
    Aeson.Array rows -> return $ vectorSource rows 
    _ -> monadThrow $ ParsingException "Not an Array"

vectorSource :: (Monad m, GVector.Vector v a) => v a -> Source m a
vectorSource vec = Conduit.sourceState (GVector.stream vec) f
  where f stream | Stream.null stream = return Conduit.StateClosed
                 | otherwise = return $ Conduit.StateOpen 
                      (Stream.tail stream) (Stream.head stream)


type RowParser a = Aeson.Value -> Either Text a

keyExistsRowParser :: (Data k) => RowParser (k, Bool)
keyExistsRowParser o @ (Aeson.Object m) 
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> pure False
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> pure False
  | Just id <- lookup "id" m,
    Just _ <- lookup "value" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> pure True
  | otherwise
    = Left $ unexpectedRowValueText o

persistedRowParser :: (Data a) => RowParser (Persisted a)
persistedRowParser o @ (Aeson.Object m) 
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just doc <- lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- lookup "_rev" docM
    = Persisted <$> fromJSON id <*> fromJSON rev <*> fromJSON doc
  | otherwise
    = Left $ unexpectedRowValueText o

maybePersistedByKeyRowParser :: (Data a, Data k) => RowParser (k, Maybe (Persisted a))
maybePersistedByKeyRowParser o @ (Aeson.Object m) 
  -- deleted
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just rev <- lookup "rev" valueM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> pure Nothing
  -- found
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just doc <- lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- lookup "_rev" docM,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> (Just <$> (Persisted <$> fromJSON id <*> fromJSON rev <*> fromJSON doc))
  -- not found
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (,) <$> fromJSON key <*> pure Nothing
  | otherwise
    = Left $ unexpectedRowValueText o


fromJSON v = case GAeson.fromJSON v of
  Aeson.Success z -> Right $ z
  Aeson.Error s -> Left $ fromString $ s

unexpectedRowValueText o
  = "Unexpected row value: " ++ (toStrict . decodeUtf8 $ Aeson.encode o)