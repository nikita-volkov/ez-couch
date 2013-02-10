{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Time where

import Prelude ()
import ClassyPrelude.Conduit
import System.Locale
import Data.Time
import qualified Network.HTTP.Types as HTTP
import Control.Monad.Reader
import EZCouch.Types
import EZCouch.Action

-- | Current time according to server.
readTime :: MonadAction m => m UTCTime 
readTime = do
  (_, _, deviation) <- ask
  localTime <- liftIO $ getCurrentTime
  return $ addUTCTime deviation localTime

getHeadersTime ((name, value) : tail) 
  | name == HTTP.hDate = case toTime value of
    Just time -> return time
    Nothing -> throwIO $ ParsingException $ "Couldn't parse date: `" ++ decodeUtf8 value ++ "`"
  | otherwise = getHeadersTime tail
getHeadersTime _ = throwIO $ ResponseException "No date header in response"

toTime = parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" 
  . unpack . asText . decodeUtf8

getTimeDeviation :: MonadAction m => m NominalDiffTime 
getTimeDeviation = do
  dbTime <- getResponseHeaders HTTP.methodGet mempty mempty mempty
    >>= getHeadersTime
  localTime <- liftIO $ getCurrentTime
  return $ diffUTCTime dbTime localTime

withTimeDeviation :: (MonadAction m) => NominalDiffTime -> m a -> m a
withTimeDeviation timeDeviation =
  local (\(settings, manager, _) -> (settings, manager, timeDeviation)) 
