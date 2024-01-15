module Puzzle9
  ( puzzle9part1
  , puzzle9part2
  ) where

import Data.List
import Data.Graph
import Data.Maybe

import Tools

addSentinels :: a -> [[a]] -> [[a]]
addSentinels s grid = (sentinelRow : map ((++[s]) . (s:)) grid) ++ [sentinelRow]
  where width  = length $ head grid
        height = length grid
        sentinelRow = take (width + 2) (repeat s)

getLows :: [[Int]] -> [Int]
getLows = map takeCenter . filter isLow . concat . map transpose . windows 3 . map (windows 3)

isLow :: [[Int]] -> Bool
isLow [[_, up, _], [left, center, right], [_, down, _]] = all (> center) [up, left, right, down]
isLow other = error (show other)

takeCenter :: [[Int]] -> Int
takeCenter = (!! 1) . (!! 1)

puzzle9part1 :: String -> String
puzzle9part1 = show . sum . map succ . getLows . addSentinels 9 . map (map (read . return)) . lines

zip' :: Int -> [(Int, Int)] -> [(Int, Int)]
zip' x ((a,b):ys) =
  let this =
        if b == 9
        then (0, 0)
        else (a, x)
  in this : zip' x ys
zip' _ _ = []

toNode :: [[(Int, Int)]] -> Maybe ((Int, Int), (Int, Int), [(Int, Int)])
toNode [[_, up, _], [left, center, right], [_, down, _]] =
  if center == (0,0)
  then Nothing
  else Just (center, center, filter (/= (0,0)) [up, left, right, down])

sccs :: [[(Int, Int)]] -> [SCC (Int, Int)]
sccs = stronglyConnComp . catMaybes . map toNode . concat . map transpose . windows 3 . map (windows 3)

puzzle9part2 :: String -> String
puzzle9part2 = show . product . take 3 . reverse . sort . map (length . flattenSCC) . sccs . addSentinels (0 :: Int, 0 :: Int) . zipWith ($) (map zip' [1..]) . map (zip [1..] . map (read . return)) . lines