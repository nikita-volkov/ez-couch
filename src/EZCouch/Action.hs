{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.Action where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Exception
import Control.Monad.Trans.Resource
import EZCouch.Types
import Network.HTTP.Types as HTTP
import Network.HTTP.Conduit as HTTP
import qualified Database.CouchDB.Conduit as DB
import qualified Database.CouchDB.Conduit.View.Query as DB
import qualified Util.Logging as Logging

log lvl = Logging.log "action" lvl

newtype Action a = Action { run :: ConnectionSettings -> Manager -> IO a }
  deriving (Functor)
instance Monad Action where
  return a = Action $ \_ _ -> return a
  a >>= b = join $ fmap b a
instance Applicative Action where
  (<*>) = ap
  pure = return

action 
  :: Method
  -- ^ Request method
  -> [ByteString]
  -- ^ Request path segments
  -> [DB.CouchQP]
  -- ^ Request arguments
  -> LByteString
  -- ^ Request body
  -> Action (ResumableSource (ResourceT IO) ByteString)
action method path qps body 
  = Action $ \settings manager -> do
      let req = request settings
      log 0 
        $ "Perfroming a " 
          ++ (show method :: Text) ++ " at " 
          ++ show (HTTP.path req ++ "?" ++ HTTP.queryString req)
      Response _ _ _ body <- runResourceT $ http req manager 
      return body
  where
    headers = [("Content-Type", "application/json")]
    query = renderQuery False $ DB.mkQuery qps
    request (ConnectionSettings host port auth database) 
      = authenticated $ def {
          method = method,
          host = host,
          requestHeaders = headers,
          port = port,
          path = DB.mkPath $ database : path,
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

putAction = action methodPut
postAction = action methodPost
getAction = action methodGet