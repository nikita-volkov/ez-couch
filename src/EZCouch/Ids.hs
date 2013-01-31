module EZCouch.Ids (generateId) where

import Prelude ()
import ClassyPrelude
import Data.Time.Clock.POSIX
import System.Random
import EZCouch.Base62

getPicos = getPOSIXTime >>= return . round . (* 1000000)
getRndSuffix l = (randomRIO (0, charsLength ^ l) :: IO Word64) >>= 
  return . zeroPad l . encodeUnsigned
  where 
    zeroPad l s = (replicate (l - length s) '0') ++ s  

generateId = (++) <$> fmap encodeUnsigned getPicos <*> getRndSuffix 3 

main = do
  generateId >>= putStrLn . pack
  generateId >>= putStrLn . pack
  generateId >>= putStrLn . pack
