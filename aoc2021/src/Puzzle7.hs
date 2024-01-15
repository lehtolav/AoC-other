module Puzzle7
  ( puzzle7part1
  , puzzle7part2
  ) where

import Data.List.Split
import Data.List
import Data.Function

import Tools

parseCrabs :: String -> [Int]
parseCrabs = map read . splitOn ","

crabDistance :: [Int] -> Int -> Int
crabDistance crabs target = sum $ map (\x -> abs $ x - target) crabs

puzzle7part1 :: String -> String
puzzle7part1 input = show $ minimumBy (compare `on` snd) $ map (\x -> (x, crabDistance crabs x)) [minimum crabs .. maximum crabs]
  where crabs = parseCrabs input

triangle :: Int -> Int
triangle x = x * (x + 1) `div` 2

crabDistance2 :: [Int] -> Int -> Int
crabDistance2 crabs target = sum $ map (\x -> triangle $ abs $ x - target) crabs

puzzle7part2 :: String -> String
puzzle7part2 input = show $ minimumBy (compare `on` snd) $ map (\x -> (x, crabDistance2 crabs x)) [minimum crabs .. maximum crabs]
  where crabs = parseCrabs input