{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Logging where

import Prelude ()
import ClassyPrelude
import qualified Util.Logging as Logging

logLn lvl = Logging.logLn lvl "EZCouch"
