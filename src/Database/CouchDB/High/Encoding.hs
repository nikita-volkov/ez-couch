{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.CouchDB.High.Encoding where

import Prelude ()
import BasicPrelude 

import Data.String (fromString)
import Data.Generics
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Aeson.Types as Aeson hiding (toJSON, fromJSON)
import qualified Data.Aeson.FixedGeneric as Aeson

docType :: (Data a) => a -> Text
docType = fromString . showConstr . head . dataTypeConstrs . dataTypeOf 

keysBody :: (Data a) => a -> LBS.ByteString
keysBody keys = "{\"keys\":" `LBS.append` Aeson.encode keys `LBS.append` "}"
