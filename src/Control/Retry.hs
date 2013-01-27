{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Retry where

import Prelude ()
import ClassyPrelude
import Control.Concurrent

retryingEither [] action = action 
retryingEither (i:is) action = action >>= processResult
  where 
    processResult (Left _) 
      | i == 0 = retryingEither is action
      | otherwise = liftIO (threadDelay i) >> retryingEither is action
    processResult r = return r

retryingEither' = retryingEither defaultIntervals

retrying exceptionIntervals action = retrying_ 0
  where
    retrying_ attempt = catch action processException
      where
        exceptionInterval = listToMaybe . drop attempt . exceptionIntervals
        processException e 
          | Just i <- exceptionInterval e
            = unless (i == 0) (liftIO (threadDelay i)) 
              >> retrying_ (attempt + 1)
          | otherwise = throwIO e

defaultIntervals = [second, second * 5, second * 15]
  where 
    second = 10 ^ 6

