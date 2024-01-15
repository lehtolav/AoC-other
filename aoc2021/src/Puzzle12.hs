module Puzzle12
  ( puzzle12part1
  , puzzle12part2
  ) where

import Data.List
import qualified Data.Map as M
import Data.Char

import Tools

parseLine :: String -> (String, String)
parseLine line = (first, tail second)
  where (first, second) = span (/= '-') line

toPathMap :: [(String, String)] -> M.Map String [String]
toPathMap paths = M.unionWith (++) (toMap paths) (toMap $ map swap paths)
  where swap (x, y) = (y, x)
        toMap = M.fromListWith (++) . map (\(x, y) -> (x, [y]))

isSmol :: String -> Bool
isSmol = all isLower

findPaths :: Int -> M.Map String [String] -> [[String]]
findPaths n paths = findPaths' n paths "start" []

findPaths' :: Int -> M.Map String [String] -> String -> [String] -> [[String]]
findPaths' n paths current trail = do
  x <- paths M.! current
  if isSmol x
  then case x of
         "end" -> [reverse (x:trail)]
         "start" -> []
         x -> resolveSpace x $ length (filter (== x) trail)
  else findPaths' n paths x (current:trail)
  where resolveSpace x l =
          if l >= n
          then []
          else if l == n - 1
               then findPaths' (max 1 (n - 1)) paths x (current:trail)
               else  findPaths' n paths x (current:trail)

puzzle12part1 :: String -> String
puzzle12part1 = show . length . findPaths 1 . toPathMap . map parseLine . lines

puzzle12part2 :: String -> String
puzzle12part2 = show . length . findPaths 2 . toPathMap . map parseLine . lines