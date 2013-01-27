{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.HTTP.Conduit.Request where

import Prelude ()
import ClassyPrelude
import Network.HTTP.Conduit

withHeader (name, value) request
  = request { requestHeaders = headers' }
  where
    headers = requestHeaders request
    headers' = (name, value) : filter ((/=) name . fst) headers

withHeaders newHeaders request
  = request { requestHeaders = headers' }
  where
    headers' = headersUnion newHeaders (requestHeaders request)

withDefaultHeaders headers request
  = request { requestHeaders = headers' }
  where
    headers' = headersUnion (requestHeaders request) headers

headersUnion headers1 headers2
  = headers1 ++ new
  where 
    namesOfExisting = asSet . fromList $ map fst headers1
    new = filter (not . flip member namesOfExisting . fst) headers2

url r = concat [
    if secure r then "https" else "http",
    "://",
    host r,
    path r,
    if null $ queryString r then "" else "?" ++ queryString r
  ]

withResponseTimeout timeout request
  = request { responseTimeout = timeout }
  
fixedHTTP request manager 
  = http request manager `catch` handleIOException
  where
    handleIOException (e :: IOException) = throwIO 
      $ FailedConnectionException 
          (unpack $ decodeUtf8 $ host request) 
          (port request)

