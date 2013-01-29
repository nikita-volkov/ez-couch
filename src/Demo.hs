{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
import Prelude ()
import ClassyPrelude
import Control.Monad
import EZCouch

connection = ConnectionSettings {
  connectionSettingsHost = "mojojojo.cloudant.com",
  connectionSettingsPort = defaultPort,
  connectionSettingsAuth = Nothing,
  connectionSettingsDatabase = "test"
}

main = run connection $ do
  replicateM_ 10 $
    EZCouch.readTime >>= liftIO . print
  