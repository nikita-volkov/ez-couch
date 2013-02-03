{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Base62 where

import Prelude ()
import ClassyPrelude
import qualified Data.List as List
import Data.Vector ((!))
import Data.Bits

chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
charsVector = asVector . fromList $ chars
charsLength = fromIntegral $ length charsVector

-- | Produces individual values for the whole range of ints including negatives. If you know that the value will allways be positive use `encodeUnsigned` instead.
encodeSigned64 = encodeUnsigned . zzEncode64 

encodeUnsigned 0 = charsVector ! 0 : []
encodeUnsigned a = if a >= 0
  then reverse . encodeUnsigned' $ a
  else error $ "EZCouch.Base62.encodeUnsigned: Negative value: " ++ show a
  where
    encodeUnsigned' 0 = []
    encodeUnsigned' a = charsVector ! fromIntegral c : encodeUnsigned' b
      where
        b = div a charsLength 
        c = mod a charsLength

zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
