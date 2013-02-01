{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric, GADTs, StandaloneDeriving #-}

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch

connection = ConnectionSettings {
  connectionSettingsHost = "127.0.0.1",
  connectionSettingsPort = defaultPort,
  connectionSettingsAuth = Nothing,
  connectionSettingsDatabase = "test"
}

data A = A {a :: Int, b :: Text, c :: Maybe Double}
  deriving (Show, Generic)
instance ToJSON A
instance FromJSON A
instance Doc A

view1 :: View A (Int, Double)
view1 = ViewKeys2 (ViewKeyField "b") (ViewKeyRandom)


regenerateAs = do
  as <- readEntities (ViewAll :: View A Text) KeysSelectionAll 0 Nothing False
  deleteMultiple as
  createMultiple $ map (\i -> A i "a" Nothing) [0..7]


main = run connection $ do
  regenerateAs
  printRandom
  printRandom
  printRandom

printRandom = do  
  as :: [Persisted A] <- readRandomEntities $ Just 2
  liftIO $ putStrLn $ 
    "Got " 
      ++ (show $ length as) ++ " random results: " 
      ++ (show $ map persistedId as)
