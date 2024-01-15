module Puzzle4
  ( puzzle4part1
  , puzzle4part2
  , puzzle4part1viz
  , puzzle4part2viz
  ) where

import Data.List

type Range = (Int, Int)

parseRange :: String -> Range
parseRange xs = (read f, read s)
  where (f, (_:s)) = break (== '-') xs

parseRanges :: String -> (Range, Range)
parseRanges xs = (parseRange f, parseRange s)
  where (f, (_:s)) = break (== ',') xs

fullyContains :: Range -> Range -> Bool
fullyContains (x1, x2) (y1, y2) = x1 <= y1 && x2 >= y2

eitherWay :: (a -> a -> Bool) -> a -> a -> Bool
eitherWay p x y = p x y || p y x

puzzle4part1 :: String -> String
puzzle4part1 = show . length . filter id . map (uncurry (eitherWay fullyContains) . parseRanges) . lines

overlap :: Range -> Range -> Bool
overlap (x1, x2) (y1, y2) = y2 >= x1 && y1 <= x2

puzzle4part2 :: String -> String
puzzle4part2 = show . length . filter id . map (uncurry overlap . parseRanges) . lines

-- Visualizations

puzzle4part1viz :: String -> String -> IO ()
puzzle4part1viz filename = const (return ())

puzzle4part2viz :: String -> String -> IO ()
puzzle4part2viz filename = const (return ())