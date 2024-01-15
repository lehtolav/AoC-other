module Puzzle9
  ( puzzle9part1
  , puzzle9part2
  , puzzle9part1viz
  , puzzle9part2viz
  ) where

import qualified Data.Set as Set
import Tools

type Point = (Int, Int)

parseDir :: Char -> Point
parseDir 'U' = (0, 1)
parseDir 'D' = (0, -1)
parseDir 'R' = (1, 0)
parseDir 'L' = (-1, 0)

parseLine :: String -> [Point]
parseLine (d:_:amt) = take (read amt) $ repeat (parseDir d)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move :: Point -> [Point] -> [Point]
move pt = scanl add pt

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

nextTo :: Int -> Int -> Int
nextTo a b =
  if a < b
  then b - 1
  else b + 1

nextTo' :: Int -> Int -> Int
nextTo' a b =
  if abs (a - b) <= 1
  then b
  else a `nextTo` b

follow :: Point -> Point -> Point
follow p1@(x1, y1) p2@(x2, y2) =
  if dist p1 p2 > 1
  then if abs (x1 - x2) > 1
       then (x1 `nextTo` x2, y1 `nextTo'` y2)
       else (x1 `nextTo'` x2, y1 `nextTo` y2)
  else p1

moves :: [Point] -> [Point] -> [[Point]]
moves [] _ = undefined
moves (point:[]) defaults = [move point defaults]
moves (p:points) defaults = scanl follow p (head rest) : rest
  where rest = moves points defaults

puzzle9part1 :: String -> String
puzzle9part1 = show . Set.size . Set.fromList . head . moves [(0, 0), (0, 0)] . concat . map parseLine . lines

puzzle9part2 :: String -> String
puzzle9part2 = show . Set.size . Set.fromList . head . moves (take 10 $ repeat (0,0)) . concat . map parseLine . lines

-- Visualizations

puzzle9part1viz :: String -> String -> IO ()
puzzle9part1viz filename = const (return ())

puzzle9part2viz :: String -> String -> IO ()
puzzle9part2viz filename = const (return ())