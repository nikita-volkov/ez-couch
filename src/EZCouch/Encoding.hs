{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Encoding where

import Prelude ()
import ClassyPrelude 
import Data.Generics
import qualified Data.ByteString.Lazy.Char8 -- export instances for LByteString
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.FixedGeneric as GAeson

docType :: (Data a) => a -> ByteString
docType = fromString . showConstr . fromMaybe undefined . listToMaybe . dataTypeConstrs . dataTypeOf 

keysBody :: (Data a) => a -> LByteString
keysBody keys = "{\"keys\":" ++ GAeson.encode keys ++ "}"

insertPairs pairs (Aeson.Object m) 
  = Aeson.Object $ fold (flip . uncurry $ insert) m pairs


