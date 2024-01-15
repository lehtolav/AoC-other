module Puzzle6
  ( puzzle6part1
  , puzzle6part2
  , puzzle6part1viz
  , puzzle6part2viz
  ) where

import Data.List
import Tools

firstUniques :: Int -> String -> Int
firstUniques x = fst . head . filter ((==x) . length . nub . snd) . zip [x..] . map (take x) . dropWhile ((<x) . length) . tails

puzzle6part1 :: String -> String
puzzle6part1 = show . firstUniques 4

puzzle6part2 :: String -> String
puzzle6part2 = show . firstUniques 14

-- Visualizations

puzzle6part1viz :: String -> String -> IO ()
puzzle6part1viz filename = const (return ())

puzzle6part2viz :: String -> String -> IO ()
puzzle6part2viz filename = const (return ())