module Puzzle2
  ( puzzle2part1
  , puzzle2part2
  ) where

import Data.Maybe

import Tools

-- Horizontal, Depth
type Place = (Int, Int)

parseCommand :: String -> Int -> Place -> Place
parseCommand "forward" = \amount -> \(h, d) -> (h + amount, d)
parseCommand "up" = \amount -> \(h, d) -> if d - amount < 0 then error "see what you did there" else (h, d - amount)
parseCommand "down" = \amount -> \(h, d) -> (h, d + amount)

parseLine :: [String] -> Place -> Place
parseLine [command, amount] = parseCommand command (read amount)

puzzle2part1 :: String -> String
puzzle2part1 = show . uncurry (*) . foldr (\f a -> f a) (0, 0) . map (parseLine . words) . lines

-- Horizontal, Depth, Aim
type Place' = (Integer, Integer, Integer)

parseCommand' :: String -> Integer -> Place' -> Place'
parseCommand' "forward" = \amount -> \(h, d, a) ->
  let depth = d + a * amount
  in if depth < 0
     then undefined
     else (h + amount, depth, a)
parseCommand' "up" = \amount -> \(h, d, a) -> (h, d, a - amount)
parseCommand' "down" = \amount -> \(h, d, a) -> (h, d, a + amount)

parseLine' :: [String] -> Place' -> Place'
parseLine' [command, amount] = parseCommand' command (read amount)

puzzle2part2 :: String -> String
puzzle2part2 = show . (\(h, d, _) -> h * d) . foldl (\a f -> f a) (0, 0, 0) . map (parseLine' . words) . lines