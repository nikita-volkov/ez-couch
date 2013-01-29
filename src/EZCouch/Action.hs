{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module EZCouch.Action where

import Prelude ()
import ClassyPrelude.Conduit
import Control.Exception (SomeException(..))
import Control.Monad.Reader
import Control.Retry
import System.IO.Error (ioeGetErrorString)
import EZCouch.Types
import Network.HTTP.Types as HTTP
import Network.HTTP.Conduit as HTTP
import Network.HTTP.Conduit.Request as HTTP
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Blaze.ByteString.Builder as Blaze
import qualified Util.Logging as Logging
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Attoparsec as Atto

logM lvl = Logging.logM lvl "EZCouch.Action"

-- | All EZCouch operations are performed in this monad.
class (MonadBaseControl IO m, MonadResource m, MonadReader (ConnectionSettings, Manager) m) => MonadAction m where

instance (MonadResource m, MonadBaseControl IO m) => MonadAction (ReaderT (ConnectionSettings, Manager) m) 

responseAction
  :: (MonadAction m) 
  => Method
  -> Maybe [Text]
  -> [CC.CouchQP]
  -> LByteString
  -> m (Response (ResumableSource m ByteString))
responseAction method dbPath qps body 
  = do
      (settings, manager) <- ask
      let request = settingsRequest settings
      logM 0 $ "Performing a " 
        ++ show method ++ " at " 
        ++ show (HTTP.url request)
      retrying exceptionIntervals $
        (flip catch) handleIOException $
          (flip catch) handleHttpException $ 
            http request manager
  where
    headers = [("Content-Type", "application/json")]
    query = renderQuery False $ CC.mkQuery qps
    settingsRequest (ConnectionSettings host port auth database) =
      authenticated $ def {
        method = method,
        host = encodeUtf8 host,
        requestHeaders = headers,
        port = port,
        path = packPath $ maybe [] (database : ) $ dbPath,
        queryString = query,
        requestBody = RequestBodyLBS body,
        checkStatus = checkStatus,
        responseTimeout = Just $ 10 ^ 6 * 10
      }
      where
        authenticated
          | Just (username, password) <- auth = applyBasicAuth (encodeUtf8 username) (encodeUtf8 password)
          | otherwise = id
    checkStatus status@(Status code message) headers
      | elem code [200, 201, 202, 304] = Nothing
      | otherwise = Just $ SomeException $ StatusCodeException status headers
    exceptionIntervals (ConnectionException {}) = [10^3, 10^6, 10^6*10]
    exceptionIntervals _ = []
    handleHttpException e = case e of
      FailedConnectionException host port -> throwIO $ ConnectionException $ 
        "FailedConnectionException: " ++ pack host ++ " " ++ show port
      otherwise -> throwIO e
    handleIOException e = throwIO $ ConnectionException $ 
      "IOError: " ++ pack (ioeGetErrorString e)

action method path qps body = do
  body <- responseBody <$> responseAction method (Just path) qps body 
  body $$+- Atto.sinkParser Aeson.json

putAction = action HTTP.methodPut
postAction = action HTTP.methodPost
getAction = action HTTP.methodGet

runWithManager manager settings action = 
  runReaderT action (settings, manager)
run settings action = HTTP.withManager $ \manager -> 
  runWithManager manager settings action

packPath = Blaze.toByteString . HTTP.encodePathSegments . filter (/="")