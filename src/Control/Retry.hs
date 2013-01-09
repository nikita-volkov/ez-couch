{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Retry where

import Prelude hiding (catch)
import Control.Exception.Lifted
import Control.Concurrent

import Control.Monad.Trans
import Control.Monad

import Data.Maybe

retryEither [] action = action 
retryEither (i:is) action = action >>= processResult
  where 
    processResult (Left _) 
      | i == 0 = retryEither is action
      | otherwise = liftIO (threadDelay i) >> retryEither is action
    processResult r = return r

retryEither' = retryEither defaultIntervals

retry exceptionIntervals action = retry_ 0
  where
    retry_ attempt = catch action processException
      where
        exceptionInterval e = listToMaybe $ drop attempt $ exceptionIntervals e
        processException e 
          | Just i <- exceptionInterval e
          = unless (i == 0) (liftIO (threadDelay i)) >> 
            retry_ (attempt + 1)
          | otherwise = throw e

defaultIntervals = [second, second * 5, second * 15]
  where 
    second = 10 ^ 6

