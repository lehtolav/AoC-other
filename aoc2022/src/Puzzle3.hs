module Puzzle3
  ( puzzle3part1
  , puzzle3part2
  , puzzle3part1viz
  , puzzle3part2viz
  ) where

import Data.Char
import Data.List
import Data.List.Split

priority :: Char -> Int
priority x
  | isLower x = ord x - 96
  | otherwise = ord x - 38

halve :: [a] -> ([a], [a])
halve xs = (take l xs, drop l xs)
  where l = length xs `div` 2

--findSame :: Eq a => [a] -> [a] -> a
--findSame xs ys = fst $ head $ filter (uncurry elem) $ zip xs (repeat ys)

puzzle3part1 :: String -> String
puzzle3part1 = show . sum . map (priority . head . uncurry intersect . halve) . lines

puzzle3part2 :: String -> String
puzzle3part2 = show . sum . map (priority . head . (\[x,y,z] -> x `intersect` y `intersect` z)) . chunksOf 3 . lines

-- Visualizations

puzzle3part1viz :: String -> String -> IO ()
puzzle3part1viz filename = const (return ())

puzzle3part2viz :: String -> String -> IO ()
puzzle3part2viz filename = const (return ())