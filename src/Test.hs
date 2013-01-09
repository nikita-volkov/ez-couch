{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction #-}

import Prelude ()
import BasicPrelude hiding (log)
import Data.Time
import System.Random
import EZCouch
import Util.PrettyPrint
import qualified Util.Logging as Logging

connection = def {
  couchHost = "mojojojo.cloudant.com"
}
db = "test"

data A = A { a :: UTCTime, b :: Maybe Double }
  deriving (Data, Typeable, Show)

generateEntitiesInDB = liftIO generateEntities >>= createMultiple db
generateEntities = sequence $ replicate 10 generateEntity
generateEntity = do
  time <- getCurrentTime
  return $ A time Nothing

purge 
  = readAll db readOptions 
    >>= \(as :: [Persisted A]) -> deleteMultiple db as

main = runCouch connection $ do
  purge
  generateEntitiesInDB
  createOrUpdateView db "De" "Vi" "AAAA" Nothing
  createOrUpdateView db "De" "Vi1" "AAAA" Nothing