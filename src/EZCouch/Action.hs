{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor #-}
module EZCouch.Action where

import Prelude ()
import ClassyPrelude.Conduit hiding (log)
import Control.Exception (SomeException(..))
import Control.Monad.Trans.Resource
import Control.Monad.Base
import EZCouch.Types
import Network.HTTP.Types as HTTP
import Network.HTTP.Conduit as HTTP
import qualified Database.CouchDB.Conduit as CC
import qualified Database.CouchDB.Conduit.View.Query as CC
import qualified Util.Logging as Logging

log lvl = Logging.log "action" lvl

data Action a b = Action { run :: ConnectionSettings -> Manager -> IO b }
instance Functor (Action a) where
  fmap f action = Action $ \s m -> fmap f $ run action s m
instance Monad (Action a) where
  return a = Action $ \_ _ -> return a
  a >>= b = Action $ \c m -> run a c m >>= \a' -> run (b a') c m
instance Applicative (Action a) where
  (<*>) = ap
  pure = return
instance MonadIO (Action a) where
  liftIO io = Action $ \_ _ -> io
instance MonadThrow (Action a) where
  monadThrow = liftIO . throwIO
instance MonadUnsafeIO (Action a) where 
  unsafeLiftIO = liftIO
instance MonadResource (Action a) where
  liftResourceT = liftIO . runResourceT
instance MonadBase IO (Action a) where
  liftBase = liftIO

-- | A helper for generic functions
actionEntityType :: Action a b -> a
actionEntityType = undefined

action 
  :: Method
  -- ^ Request method
  -> [ByteString]
  -- ^ Request path segments
  -> [CC.CouchQP]
  -- ^ Request arguments
  -> LByteString
  -- ^ Request body
  -> Action a (ResumableSource (ResourceT IO) ByteString)
  -- ^ An action over entity 'a'
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

putAction = action methodPut
postAction = action methodPost
getAction = action methodGet