{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module EZCouch.Action where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Exception (SomeException(..))
import Control.Monad.Reader
import EZCouch.Types
import Network.HTTP.Types as HTTP
import Network.HTTP.Conduit as HTTP
import qualified Database.CouchDB.Conduit as CC
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Util.Logging as Logging

log lvl = Logging.log "action" lvl

class (MonadBaseControl IO m, MonadResource m, MonadReader (ConnectionSettings, Manager) m) => MonadAction m where

instance (MonadResource m, MonadBaseControl IO m) => MonadAction (ReaderT (ConnectionSettings, Manager) m) 

action
  :: (MonadAction m) 
  => Method
  -> [CC.Path]
  -> [CC.CouchQP]
  -> LByteString
  -> m (ResumableSource m ByteString)
action method path qps body 
  = do
      (settings, manager) <- ask
      let req = request settings
      log 0 
        $ "Perfroming a " 
          ++ show method ++ " at " 
          ++ show (HTTP.path req ++ "?" ++ HTTP.queryString req)
      Response _ _ _ body <- http req manager 
      return body
  where
    headers = [("Content-Type", "application/json")]
    query = renderQuery False $ CC.mkQuery qps
    request (ConnectionSettings host port auth database) 
      = authenticated $ def {
          method = method,
          host = host,
          requestHeaders = headers,
          port = port,
          path = CC.mkPath $ database : path,
          queryString = query,
          requestBody = RequestBodyLBS body,
          checkStatus = checkStatus
          -- responseTimeout = Just $ 10 ^ 6 * 30
        }
      where
        authenticated
          | Just (username, password) <- auth = applyBasicAuth username password
          | otherwise = id

    checkStatus status@(Status code message) headers
      | elem code [200, 201, 202, 304] = Nothing
      | otherwise = Just $ SomeException $ StatusCodeException status headers

putAction = action HTTP.methodPut
postAction = action HTTP.methodPost
getAction = action HTTP.methodGet


runWithManager manager settings action = runReaderT action (settings, manager)
run settings action = HTTP.withManager $ 
  \manager -> runWithManager manager settings action

