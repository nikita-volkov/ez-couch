module Database.CouchDB.High.Ids (generateId) where

import Data.Char
import Data.IntMap (fromList, (!))
import Data.Time.Clock.POSIX
import System.Random
import Control.Applicative

chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
charsLength = length chars
charsMap = fromList $ zip [0..charsLength] chars

encode = reverse . encode_
  where
    encode_ a 
      | a < charsLength = charsMap ! a : []
      | otherwise = charsMap ! c : encode_ b
      where
        b = div a charsLength 
        c = mod a charsLength

getPicos = getPOSIXTime >>= return . round . (* 1000000)
getRndSuffix l = randomRIO (0, charsLength ^ l) >>= return . zeroPad l . encode
  where 
    zeroPad l s = (replicate (l - length s) '0') ++ s  

generateId = (++) <$> fmap encode getPicos <*> getRndSuffix 3 

main = do
  generateId >>= putStrLn 
  generateId >>= putStrLn 
  generateId >>= putStrLn 
  generateId >>= putStrLn 
