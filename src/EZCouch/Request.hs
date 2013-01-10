{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module EZCouch.Request where

import Prelude ()
import BasicPrelude hiding (log)
import Control.Exception.Lifted 

import qualified Data.Aeson as Aeson hiding (toJSON, fromJSON)
import qualified Database.CouchDB.Conduit as DB
import qualified Database.CouchDB.Conduit.LowLevel as DB
import qualified Database.CouchDB.Conduit.View.Query as DB
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Util as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.Conduit.Attoparsec as Atto
import qualified Data.Attoparsec as Atto

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Fusion.Stream as Stream

import EZCouch.Types

import qualified Util.Logging as Logging

log lvl = Logging.log "request" lvl

-- | An HTTP-method-agnostic request. Based on QPKeys it decides which method to use and whether to filter out the QPStartKey and QPEndKey.
smartRead path qps
  | Just keysJSON <- qpsKeysJSON qps,
    body <- "{\"keys\":" `LBS.append` keysJSON `LBS.append` "}",
    qps' <- filter f qps
  = request readRowsSink HTTP.methodPost path qps' body
  | otherwise
  = request readRowsSink HTTP.methodGet path qps ""
  where
    f (DB.QPKeys _) = False
    f (DB.QPStartKey _) = False
    f (DB.QPEndKey _) = False
    f _ = True
    qpsKeysJSON (DB.QPKeys keys : _) = Just $ Aeson.encode keys
    qpsKeysJSON (_ : tail) = qpsKeysJSON tail
    qpsKeysJSON _ = Nothing


postRead = request readRowsSink HTTP.methodPost
getRead = request readRowsSink HTTP.methodGet
getReadOne = request sink HTTP.methodGet
  where
    sink = Atto.sinkParser (Aeson.json Atto.<?> "json object")
postUpdate = request updateRowsSink HTTP.methodPost

request 
  :: DB.MonadCouch m 
  => Conduit.Sink ByteString m a
  -- ^ Sink function implementation
  -> HTTP.Method
  -- ^ Request method
  -> DB.Path
  -- ^ Request path
  -> [DB.CouchQP]
  -- ^ Request arguments
  -> LBS.ByteString
  -- ^ Request body
  -> m a
request sink method path qps body 
  = do
      log 0 $ "Perfroming a " ++ (read . show $ method :: Text) ++ " at " ++ (read . show $ path) ++ (read . show $ HTTP.renderQuery True query)
      HTTP.Response _ _ _ src 
        <- DB.couch method path headers query (HTTP.RequestBodyLBS body) DB.protect'
      src Conduit.$$+- sink
  where
    headers = [("Content-Type", "application/json")]
    query = DB.mkQuery qps


readRowsSink :: Conduit.MonadResource m => Conduit.Sink ByteString m (Conduit.Source m Aeson.Value)
readRowsSink = do 
  o <- Atto.sinkParser (Aeson.json Atto.<?> "json object")
  rows <- case o of
      (Aeson.Object raw') -> case HashMap.lookup "rows" raw' of
          (Just (Aeson.Array r)) -> return r
          _ -> return GVector.empty
      _ -> throw $ DB.CouchInternalError "view entry is not an object"
  return $ vectorSource rows
  where
    valToObj (Aeson.Object o) = o
    valToObj _ = throw $ DB.CouchInternalError "row is not object"

updateRowsSink :: Conduit.MonadResource m => Conduit.Sink ByteString m (Conduit.Source m Aeson.Value)
updateRowsSink = do 
  Aeson.Array rows <- Atto.sinkParser (Aeson.json Atto.<?> "json object")
  return $ vectorSource rows 

vectorSource :: (Monad m, GVector.Vector v a) => v a -> Conduit.Source m a
vectorSource vec = Conduit.sourceState (GVector.stream vec) f
  where f stream | Stream.null stream = return Conduit.StateClosed
                 | otherwise = return $ Conduit.StateOpen 
                      (Stream.tail stream) (Stream.head stream)
