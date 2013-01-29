{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
import Prelude ()
import ClassyPrelude
import Control.Monad
import Data.Time
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import EZCouch
import Util.PrettyPrint
import qualified Util.Logging as Logging
import Debug.Trace

logM lvl = Logging.logM lvl "test"

connection = ConnectionSettings {
  connectionSettingsHost = "mojojojo.cloudant.com",
  connectionSettingsPort = defaultPort,
  connectionSettingsAuth = Nothing,
  connectionSettingsDatabase = "test"
}

data A = A { a :: UTCTime, b :: Maybe Double }
  deriving (Data, Typeable, Show, Generic)
instance ToJSON A
instance FromJSON A
instance Doc A

generateEntitiesInDB = liftIO generateEntities >>= createMultiple
generateEntities = sequence $ replicate 10 generateEntity
generateEntity = do
  time <- getCurrentTime
  return $ A time Nothing

purge = do
  logM 0 "Purging"
  as :: [Persisted A] <- readMultiple readOptions
  logM 1 "Purged " ++ (show $ length $ as)
  deleteMultiple as

testConnectionAlive i = trace (show i) $ if i > 0
  then do
    entities <- liftIO $ sequence $ replicate 100 generateEntity
    createMultiple entities
    testConnectionAlive $ i - 1
  else return ()

  
main = run connection $ do
  liftIO $ Logging.initialize
  replicateM_ 10 $
    EZCouch.readTime >>= liftIO . print
  