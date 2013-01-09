{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.CouchDB.High.Parsing where

import Prelude ()
import BasicPrelude
import Control.Exception.Lifted 
import Data.String (fromString)

import Data.Generics
import qualified Data.HashMap.Lazy as HashMap
import Database.CouchDB.High.Types
import qualified Data.Aeson.Types as StaticAeson 
import qualified Data.Aeson.Encode as StaticAeson
import qualified Data.Aeson.Types as Aeson hiding (toJSON, fromJSON)
import qualified Data.Aeson.FixedGeneric as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS

rowToIdEither o @ (Aeson.Object m) 
  | Just rev <- HashMap.lookup "rev" m,
    Just id <- HashMap.lookup "id" m
    = Right $ (id, rev)
  | Just code <- HashMap.lookup "error" m,
    Just reason <- HashMap.lookup "reason" m,
    Just id <- HashMap.lookup "id" m
    = Left $ id
  | otherwise
    = throwUnexpectedRowValueException o

rowToBool o @ (Aeson.Object m) 
  | Just id <- HashMap.lookup "id" m,
    Just (Aeson.Object valueM) <- HashMap.lookup "value" m,
    Just (Aeson.Bool True) <- HashMap.lookup "deleted" valueM,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, True)
  | Just id <- HashMap.lookup "id" m,
    Just _ <- HashMap.lookup "value" m,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, True)
  | Just "not_found" <- HashMap.lookup "error" m,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, False)
  | otherwise
    = throwUnexpectedRowValueException o

rowToPersisted o @ (Aeson.Object m) 
  | Just id <- HashMap.lookup "id" m,
    Just (Aeson.Object valueM) <- HashMap.lookup "value" m,
    Just doc <- HashMap.lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- HashMap.lookup "_rev" docM
    = Persisted (fromJSON id) (fromJSON rev) (fromJSON doc)
  | otherwise
    = throwUnexpectedRowValueException o

rowToMaybePersistedByKey o @ (Aeson.Object m) 
  -- deleted
  | Just id <- HashMap.lookup "id" m,
    Just (Aeson.Object valueM) <- HashMap.lookup "value" m,
    Just (Aeson.Bool True) <- HashMap.lookup "deleted" valueM,
    Just rev <- HashMap.lookup "rev" valueM,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, Nothing)
  -- found
  | Just id <- HashMap.lookup "id" m,
    Just (Aeson.Object valueM) <- HashMap.lookup "value" m,
    Just doc <- HashMap.lookup "doc" m,
    Aeson.Object docM <- doc,
    Just rev <- HashMap.lookup "_rev" docM,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, Just (Persisted (fromJSON id) (fromJSON rev) (fromJSON doc)))
  -- not found
  | Just "not_found" <- HashMap.lookup "error" m,
    Just key <- HashMap.lookup "key" m
    = (fromJSON key, Nothing)
  | otherwise
    = throwUnexpectedRowValueException o


fromJSON v = case Aeson.fromJSON v of
  Aeson.Success z -> z
  Aeson.Error s -> throw $ ParsingException $ fromString $ s

throwUnexpectedRowValueException o
  = throw $ ParsingException $ "Unexpected row value: " ++ (fromString . LBS.unpack $ StaticAeson.encode o)