{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Time where

import Prelude ()
import ClassyPrelude.Conduit
import System.Locale
import Data.Time
import Data.Time.Format
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import EZCouch.Types
import EZCouch.Action

-- | Current time according to server.
readTime :: MonadAction m => m UTCTime 
readTime = responseAction HTTP.methodGet mempty mempty mempty
  >>= getHeadersTime . HTTP.responseHeaders

getHeadersTime ((name, value) : tail) 
  | name == HTTP.hDate = case toTime value of
    Just time -> return time
    Nothing -> throwIO $ ParsingException $ "Couldn't parse date: `" ++ decodeUtf8 value ++ "`"
  | otherwise = getHeadersTime tail
getHeadersTime _ = throwIO $ ServerException "No date header in response"

toTime = parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" 
  . unpack . asText . decodeUtf8
