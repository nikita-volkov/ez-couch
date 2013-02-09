{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Crash where

import Prelude ()
import ClassyPrelude


crash msg = error $ unpack $ asText $ "EZCouch bug occurred, please report it. " ++ msg
