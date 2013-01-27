{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Control.Retry where

import Prelude ()
import ClassyPrelude
import Control.Concurrent
import qualified Util.Logging as Logging


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
          | Just i <- exceptionInterval e = do
              Logging.log "Control.Retry" 3 
                $ "Error occurred: " ++ show e ++ ". " 
                  ++ "Retrying with a " ++ show (i `div` sec) ++ "s delay."
              unless (i == 0) (liftIO (threadDelay i)) 
              retrying_ (attempt + 1)
          | otherwise = throwIO e

defaultIntervals = [sec, sec * 5, sec * 15]
sec = 10 ^ 6

