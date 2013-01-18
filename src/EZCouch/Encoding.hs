{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Encoding where

import Prelude ()
import ClassyPrelude 
import Data.ByteString.Lazy.Char8 () -- export instances for LByteString
import Data.Aeson as Aeson

keysBody :: (ToJSON a) => a -> LByteString
keysBody keys = "{\"keys\":" ++ encode keys ++ "}"

insertPairs pairs (Object m) 
  = Object $ fold (flip . uncurry $ insert) m pairs


