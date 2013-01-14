{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
import Prelude ()
import ClassyPrelude hiding (log)
import Data.Time
import System.Random
import Control.Concurrent
import EZCouch
import Util.PrettyPrint
import qualified Util.Logging as Logging
import Debug.Trace

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
  -- generateEntitiesInDB
  testConnectionAlive 0
  -- createOrUpdateView db "De" "Vi" "AAAA" Nothing
  -- createOrUpdateView db "De" "Vi1" "AAAA" Nothing

testConnectionAlive i = trace (show i) $ if i < 1000
  then do
    entity <- liftIO $ generateEntity
    create db entity
    testConnectionAlive $ i + 1
  else return ()