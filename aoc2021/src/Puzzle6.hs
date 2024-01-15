module Puzzle6
  ( puzzle6part1
  , puzzle6part2
  ) where

import Data.List.Split

import Tools

parseFish :: String -> [Int]
parseFish = map read . splitOn ","

spawnTimer :: Int -> Int
spawnTimer 0 = 6
spawnTimer x = x - 1

simulate :: [Int] -> [Int]
simulate now = take (news now) (repeat 8) ++ map spawnTimer now
  where news = length . filter (== 0)

-- Better solution, required for part 2

count :: [Int] -> Int -> Int
count xs x = length $ filter (== x) $ xs

fishNums :: [Int] -> [Int]
fishNums fish = map (count fish) [0..6] ++ [0, 0]

simulate2 :: [Int] -> [Int]
simulate2 fish = tail old ++ (spawn + head new) : (tail new ++ [spawn])
  where (old, new) = splitAt 7 fish
        spawn = head old

puzzle6part1 :: String -> String
puzzle6part1 = show . sum . (!! 80) . iterate simulate2 . fishNums . parseFish

puzzle6part2 :: String -> String
puzzle6part2 = show . sum . (!! 256) . iterate simulate2 . fishNums . parseFish