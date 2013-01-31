{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Util.Logging where

import Prelude (String)
import ClassyPrelude 

import System.Log.Logger as Logger 
import System.Log 
import System.Log.Handler 
import System.Log.Formatter
import System.Log.Handler.Simple hiding (formatter) 
import System.IO

import System.Locale (defaultTimeLocale)
import Data.Time (getZonedTime, getCurrentTime, formatTime)
import Control.Concurrent (myThreadId)

logM :: (MonadIO m) => Int -> Text -> Text -> m ()
logM level logger message = liftIO $ 
  Logger.logM (unpack logger) (levelPriority level) (unpack message)

initialize = initializeWithFormat "$level $time, $logger: $message"

initializeWithFormat :: Text -> IO ()
initializeWithFormat format = do
  updateGlobalLogger "" (Logger.setLevel DEBUG)

  removeAllHandlers
  h <- streamHandler stderr DEBUG
  h <- return $ setFormatter h (formatter format)
  updateGlobalLogger "" (setHandlers [h])

setLoggerLevel logger level = do
  updateGlobalLogger (unpack logger) (Logger.setLevel $ levelPriority level)

levelPriority level = case level of
  0 -> DEBUG
  1 -> INFO
  2 -> NOTICE
  3 -> WARNING
  4 -> ERROR
  5 -> CRITICAL
  6 -> ALERT
  x | x >= 7 -> EMERGENCY

priorityLevel p = fromMaybe undefined $ find ((==) p . levelPriority) [0..7]

formatter format h (prio, msg) loggername 
  = replaceVarM 
      [ 
        ("time", formatTime defaultTimeLocale timeFormat <$> getZonedTime),
        ("utcTime", formatTime defaultTimeLocale timeFormat <$> getCurrentTime),
        ("message", return msg), 
        ("priority", return $ show prio), 
        ("level", return $ show $ priorityLevel prio), 
        ("logger", return loggername), 
        ("tid", show <$> myThreadId)
      ]
      $ unpack format
  where
    timeFormat = "%F %X %Z"


-- | Replace some '$' variables in a string with supplied values
replaceVarM :: [(String, IO String)] -- ^ A list of (variableName, action to get the replacement string) pairs
           -> String   -- ^ String to perform substitution on
           -> IO String   -- ^ Resulting string
replaceVarM _ [] = return []
replaceVarM keyVals (s:ss) | s=='$' = do (f,rest) <- replaceStart keyVals ss
                                         repRest <- replaceVarM keyVals rest
                                         return $ f ++ repRest
                           | otherwise = replaceVarM keyVals ss >>= return . (s:)
    where
      replaceStart [] str = return ("$",str)
      replaceStart ((k,v):kvs) str | k `isPrefixOf` str = do vs <- v
                                                             return (vs, drop (length k) str)
                                   | otherwise = replaceStart kvs str
                