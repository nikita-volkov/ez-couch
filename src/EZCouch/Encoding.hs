{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Encoding where

import Prelude ()
import ClassyPrelude 
import Data.Generics
import qualified Data.ByteString.Lazy.Char8 -- export instances for LByteString
import qualified Data.Aeson.FixedGeneric as Aeson

docType :: (Data a) => a -> ByteString
docType = fromString . showConstr . maybe undefined id . listToMaybe . dataTypeConstrs . dataTypeOf 

keysBody :: (Data a) => a -> LByteString
keysBody keys = "{\"keys\":" ++ Aeson.encode keys ++ "}"
