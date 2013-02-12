{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module EZCouch.Action where

import Prelude ()
import ClassyPrelude.Conduit
import Control.Exception (SomeException(..))
import Control.Monad.Reader
import System.IO.Error (ioeGetErrorString)
import EZCouch.Types
import EZCouch.Logging
import EZCouch.Retry
import EZCouch.Crash
import Data.Time
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Conduit.Request as HTTP
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Attoparsec as Atto
import qualified EZCouch.Model.Error as Error

data ConnectionSettings 
  = ConnectionSettings {  
      connectionSettingsHost :: Text,
      connectionSettingsPort :: Int,
      connectionSettingsAuth :: Maybe (Text, Text),
      connectionSettingsDatabase :: Text
    }

defaultPort = 5984 :: Int

type Environment = (ConnectionSettings, HTTP.Manager, NominalDiffTime)

-- | All EZCouch operations are performed in this monad.
class (MonadBaseControl IO m, MonadResource m, MonadReader Environment m) => MonadAction m where

instance (MonadResource m, MonadBaseControl IO m) => MonadAction (ReaderT Environment m) 

generateRequest :: (MonadAction m) 
  => HTTP.Method
  -> Maybe [Text]
  -> [CC.CouchQP]
  -> LByteString
  -> m (HTTP.Request m)
generateRequest method dbPath qps body = do
  (settings, _, _) <- ask
  return $ settingsRequest settings
  where
    headers = [("Content-Type", "application/json")]
    query = HTTP.renderQuery False $ CC.mkQuery qps
    settingsRequest (ConnectionSettings host port auth database) =
      authenticated $ HTTP.def {
        HTTP.method = method,
        HTTP.host = encodeUtf8 host,
        HTTP.requestHeaders = headers,
        HTTP.port = port,
        HTTP.path = packPath $ maybe [] (database : ) $ dbPath,
        HTTP.queryString = query,
        HTTP.requestBody = HTTP.RequestBodyLBS body,
        HTTP.checkStatus = \_ _ -> Nothing,
        HTTP.responseTimeout = Just $ 10 ^ 6 * 5
      }
      where
        authenticated
          | Just (username, password) <- auth = HTTP.applyBasicAuth (encodeUtf8 username) (encodeUtf8 password)
          | otherwise = id

performRequest :: (MonadAction m) 
  => HTTP.Request m
  -> m (Response (HTTP.Response (UnparsedBody m)))
performRequest request = do
  logLn 0 $ "Performing a " 
    ++ show (HTTP.method request) 
    ++ " at " ++ show (HTTP.url request)
  (_, manager, _) <- ask
  retrying exceptionIntervals $ 
    processResponse =<< http' request manager
  where
    exceptionIntervals (ConnectionException {}) = [0]
    exceptionIntervals (ServerException {}) = [0]
    exceptionIntervals _ = []

http' request manager = 
  (flip catch) handleIOException $
    (flip catch) handleHttpException $ 
      HTTP.http request manager
  where
    handleHttpException e = case e of
      HTTP.FailedConnectionException host port -> throwIO $ ConnectionException $ 
        "FailedConnectionException: " ++ pack host ++ " " ++ show port
      HTTP.ResponseTimeout -> throwIO $ ConnectionException $ "ResponseTimeout"
      otherwise -> throwIO e
    handleIOException e = throwIO $ ConnectionException $ 
      "IOError: " ++ pack (ioeGetErrorString e)

getResponseHeaders method path qps body = do
  response <- performRequest =<< generateRequest method path qps body 
  case response of
    ResponseNotFound -> crash $ "Getting headers from a Not Found response"
    ResponseOk response -> do
      HTTP.responseBody response $$+- return ()
      return $ HTTP.responseHeaders response

getResponseJSON method path qps body = do
  response <- performRequest =<< generateRequest method path qps body 
  case response of
    ResponseOk response -> do
      json <- HTTP.responseBody response $$+- Atto.sinkParser Aeson.json
      return $ ResponseOk json
    ResponseNotFound -> return ResponseNotFound

putAction path = getResponseJSON HTTP.methodPut (Just path)
postAction path = getResponseJSON HTTP.methodPost (Just path)
getAction path = getResponseJSON HTTP.methodGet (Just path)

packPath = Blaze.toByteString . HTTP.encodePathSegments . filter (/="")

type UnparsedBody m = ResumableSource m ByteString

-- | 
data Response r =
  ResponseNotFound |
  ResponseOk r

processResponse :: MonadAction m
  => HTTP.Response (UnparsedBody m) -> m (Response (HTTP.Response (UnparsedBody m)))
processResponse response@(HTTP.Response (HTTP.Status code msg) _ headers body) =
  case code of
    -- Handle status 500 by extracting a possible "Not found response" or 
    -- throwing a ServerException otherwise
    _ | code `elem` [404, 500] -> do
      json <- body $$+- Atto.sinkParser Aeson.json
      case Aeson.fromJSON json of
        Aeson.Success (Error.Error "error" (Just reason) _)
          | isPrefixOf "{{try_clause,{not_found,missing}}" reason 
          -> return ResponseNotFound
        Aeson.Success (Error.Error "not_found" (Just reason) _)
          -> return ResponseNotFound
        Aeson.Success _ -> 
          throwIO $ ServerException $ "Status " ++ show code ++ " response: " ++ (decodeUtf8 . toStrict . Aeson.encode) json
        Aeson.Error m -> 
          throwIO $ ServerException $ "Status " ++ show code
    _ | code >= 400 ->
      crash $ "Unexpected status code: " ++ show code ++ ", " ++ (decodeUtf8) msg
    _ -> return $ ResponseOk response
