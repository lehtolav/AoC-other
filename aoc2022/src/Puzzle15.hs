module Puzzle15
  ( puzzle15part1
  , puzzle15part2
  ) where

import Data.Char
import Data.List
import Data.List.Split

import Tools

type Point = (Int, Int)

parseSensor :: String -> (Point, Point)
parseSensor line = ((x1, y1), (x2, y2))
  where [x1, y1, x2, y2] = map (read . takeWhile (\x -> isDigit x || x == '-')) . tail . splitOn "=" $ line

diff :: Int -> Int -> Int
diff x y = abs (x - y)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = diff x1 x2 + diff y1 y2

atY :: Int -> (Point, Point) -> [Int]
atY y (sensor, beacon) = map (+ fst sensor) . spread $ max 0 $ d - (diff y (snd sensor))
  where d = manhattan sensor beacon
        spread x = if x < 0 then [] else [negate x .. x]

puzzle15part1 :: String -> String
puzzle15part1 = show . length . group . sort . concat . take 1 . map (atY 2000000 . parseSensor) . lines

puzzle15part2 :: String -> String
puzzle15part2 = id
