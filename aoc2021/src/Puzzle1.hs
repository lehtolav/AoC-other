module Puzzle1
  ( puzzle1part1
  , puzzle1part2
  ) where

import Data.Maybe
import Data.List

import Tools

increases :: [Int] -> Bool
increases (x:y:_) = x < y
increases _ = False

--puzzle1part1 :: String -> String
--puzzle1part1 = show . length . filter id . extend' increases . map read . lines

puzzle1part1 :: String -> String
puzzle1part1 = show . length . filter id . map (\(x:y:_) -> y > x) . filter ((> 1) . length) . tails . map (read :: String -> Int) . lines


window :: [a] -> Maybe [a]
window (x:y:z:_) = Just [x, y, z]
window _ = Nothing

--puzzle1part2 :: String -> String
--puzzle1part2 = show . length . filter id . extend' increases . map sum . catMaybes . extend' window . map readInt . lines

puzzle1part2 :: String -> String
puzzle1part2 = show . length . filter id . map (\(x:y:_) -> y > x) . filter ((> 1) . length) . tails . map sum . filter ((== 3) . length) . map (take 3) . tails . map (read :: String -> Int) . lines
