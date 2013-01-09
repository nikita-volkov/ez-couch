{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.Utils where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Aeson.Types as Aeson 

insert pairs (Aeson.Object m) 
  = Aeson.Object $ foldr (uncurry HashMap.insert) m pairs
empty = Aeson.object []
(!) (Aeson.Object m) k = m HashMap.! k


