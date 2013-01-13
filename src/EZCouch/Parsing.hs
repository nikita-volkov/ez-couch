{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Parsing where

import Prelude ()
import ClassyPrelude
import Control.Exception.Lifted 
import Data.Text.Lazy (toStrict)
import Data.Generics
import EZCouch.Types
import qualified Data.Aeson.Types as StaticAeson 
import qualified Data.Aeson.Encode as StaticAeson
import qualified Data.Aeson.Types as Aeson hiding (toJSON, fromJSON)
import qualified Data.Aeson.FixedGeneric as Aeson

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
  = throw $ ParsingException $ "Unexpected row value: " ++ (toStrict . decodeUtf8 $ StaticAeson.encode o)