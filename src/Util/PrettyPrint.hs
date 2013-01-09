{-# LANGUAGE DeriveDataTypeable #-}
module Util.PrettyPrint (tree, Data, Typeable) where

import Control.Applicative
import Data.Tree
import Data.Generics
import Data.String
import qualified Data.Text as Text

dataTree :: Data a => a -> Tree String
dataTree = fix . genericTree
  where
    genericTree :: Data a => a -> Tree String
    genericTree = dflt `extQ` text `extQ` string
      where
        text x = Node (Text.unpack x) []
        string x = Node x []
        dflt a = Node (showConstr (toConstr a)) (gmapQ genericTree a)
    fix (Node name forest)
      | name == "(:)" 
      , a : b : [] <- forest
        = Node ":" $ (fix a) : (subForest $ fix b)
      | name == "(,)" = Node "," $ fix <$> forest
      | otherwise = Node name $ fix <$> forest


tree :: (Data a, IsString b) => a -> b
tree = fromString . unlines . draw . dataTree
  where
    draw :: Tree String -> [String]
    draw (Node x ts0) = x : drawSubTrees ts0
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            shift "- " "  " (draw t)
        drawSubTrees (t:ts) =
            shift "- " "| " (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)




-- data SomeType = A [String] Int | B | C Int | D [[String]] 
--   deriving (Typeable, Data)

-- xxx = A ["a", "b", "c"] 9 
--     : C 3 
--     : B 
--     : D [["asdf", "123", "ldskfjkl"], ["f"]]
--     : []

-- main = do
--   putStrLn $ tree $ dataTree xxx
