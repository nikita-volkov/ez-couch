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


rowToIdEither o @ (Aeson.Object m) 
  | Just rev <- lookup "rev" m,
    Just id <- lookup "id" m
    = Right $ (id, rev)
  | Just code <- lookup "error" m,
    Just reason <- lookup "reason" m,
    Just id <- lookup "id" m
    = Left $ id
  | otherwise
    = throwUnexpectedRowValueException o

rowToBool o @ (Aeson.Object m) 
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just key <- lookup "key" m
    = (fromJSON key, True)
  | Just id <- lookup "id" m,
    Just _ <- lookup "value" m,
    Just key <- lookup "key" m
    = (fromJSON key, True)
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (fromJSON key, False)
  | otherwise
    = throwUnexpectedRowValueException o

rowToPersisted o @ (Aeson.Object m) 
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just doc <- lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- lookup "_rev" docM
    = Persisted (fromJSON id) (fromJSON rev) (fromJSON doc)
  | otherwise
    = throwUnexpectedRowValueException o

rowToMaybePersistedByKey o @ (Aeson.Object m) 
  -- deleted
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just (Aeson.Bool True) <- lookup "deleted" valueM,
    Just rev <- lookup "rev" valueM,
    Just key <- lookup "key" m
    = (fromJSON key, Nothing)
  -- found
  | Just id <- lookup "id" m,
    Just (Aeson.Object valueM) <- lookup "value" m,
    Just doc <- lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- lookup "_rev" docM,
    Just key <- lookup "key" m
    = (fromJSON key, Just (Persisted (fromJSON id) (fromJSON rev) (fromJSON doc)))
  -- not found
  | Just "not_found" <- lookup "error" m,
    Just key <- lookup "key" m
    = (fromJSON key, Nothing)
  | otherwise
    = throwUnexpectedRowValueException o


fromJSON v = case Aeson.fromJSON v of
  Aeson.Success z -> z
  Aeson.Error s -> throw $ ParsingException $ fromString $ s

throwUnexpectedRowValueException o
  = throw $ ParsingException $ "Unexpected row value: " ++ (toStrict . decodeUtf8 $ Aeson.encode o)