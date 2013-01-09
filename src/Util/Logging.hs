module Util.Logging where

import Prelude ()
import BasicPrelude 

import System.Log.Logger as Logger 
import System.Log 
import System.Log.Handler 
import System.Log.Formatter
import System.Log.Handler.Simple hiding (formatter)
import System.IO

import System.Locale (defaultTimeLocale)
import Data.Time (getZonedTime, getCurrentTime, formatTime)
import Control.Concurrent (myThreadId)

initialize = do
  updateGlobalLogger "" (Logger.setLevel DEBUG)

  removeAllHandlers
  h <- streamHandler stderr DEBUG
  h <- return $ setFormatter h (formatter "$level $time, $logger: $message")
  updateGlobalLogger "" (setHandlers [h])

log logger level message
  = liftIO $ logM (textToString logger) (levelPriority level) (textToString message)

setLoggerLevel logger level = do
  updateGlobalLogger (textToString logger) (Logger.setLevel $ levelPriority level)

levelPriority level = case level of
  0 -> DEBUG
  1 -> INFO
  2 -> NOTICE
  3 -> WARNING
  4 -> ERROR
  5 -> CRITICAL
  6 -> ALERT
  x | x >= 7 -> EMERGENCY

priorityLevel p = head [l | l <- [0..7], p == levelPriority l]

formatter format h (prio, msg) loggername 
  = replaceVarM 
      [ 
        ("time", formatTime defaultTimeLocale timeFormat <$> getZonedTime),
        ("utcTime", formatTime defaultTimeLocale timeFormat <$> getCurrentTime),
        ("message", return msg), 
        ("priority", return $ textToString $ show prio), 
        ("level", return $ textToString $ show $ priorityLevel prio), 
        ("logger", return loggername), 
        ("tid", textToString <$> show <$> myThreadId)
      ]
      format
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
                