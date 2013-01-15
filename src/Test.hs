{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
import Prelude ()
import ClassyPrelude hiding (log)
import Data.Time
import Control.Concurrent
import EZCouch
import Util.PrettyPrint
import qualified Util.Logging as Logging
import Debug.Trace

connection = ConnectionSettings {
  connectionSettingsHost = "mojojojo.cloudant.com",
  connectionSettingsPort = defaultPort,
  connectionSettingsAuth = Nothing,
  connectionSettingsDatabase = "test"
}

data A = A { a :: UTCTime, b :: Maybe Double }
  deriving (Data, Typeable, Show)

generateEntitiesInDB = liftIO generateEntities >>= createMultiple
generateEntities = sequence $ replicate 10 generateEntity
generateEntity = do
  time <- getCurrentTime
  return $ A time Nothing

purge = do
  as :: [Persisted A] <- readMultiple readOptions
  liftIO $ print $ length as 
  deleteMultiple as

-- main = getCurrentTime >>= print
main = run' connection $ do
  purge
--   -- generateEntitiesInDB
  testConnectionAlive 3
--   -- createOrUpdateView db "De" "Vi" "AAAA" Nothing
--   -- createOrUpdateView db "De" "Vi1" "AAAA" Nothing

testConnectionAlive i = trace (show i) $ if i > 0
  then do
    entity <- liftIO $ generateEntity
    create entity
    testConnectionAlive $ i - 1
  else return ()