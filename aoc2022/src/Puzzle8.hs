module Puzzle8
  ( puzzle8part1
  , puzzle8part2
  , puzzle8part1viz
  , puzzle8part2viz
  ) where

import Data.List

visible :: Int -> [(Int, Bool)] -> [(Int, Bool)]
visible _ [] = []
visible h ((t, v):xs) = (t, v') : visible h' xs
  where h' = max h t
        v' = v || t > h

puzzle8part1 :: String -> String
puzzle8part1 = show . sum . map (length . filter snd) . everyAngle (visible (-1)) . map ((flip zip) (repeat False) . map (read . return)) . lines

everyAngle :: ([a] -> [a]) -> [[a]] -> [[a]]
everyAngle f = map (f . reverse . f) . transpose . map (f . reverse . f)

views :: [(Int, [Int])] -> [(Int, [Int])]
views [] = []
views ((h,vs):xs) = (h, view h xs : vs) : views xs

view :: Int -> [(Int, a)] -> Int
view h [] = 0
view h xs = length smaller + last
  where (smaller, rest) = span (<h) $ map fst $ xs
        last = if null rest then 0 else 1

puzzle8part2 :: String -> String
puzzle8part2 = show . maximum . map (maximum . map (product . snd)) . everyAngle views . map ((flip zip) (repeat []) . map (read . return)) . lines

-- Visualizations

puzzle8part1viz :: String -> String -> IO ()
puzzle8part1viz filename = const (return ())

puzzle8part2viz :: String -> String -> IO ()
puzzle8part2viz filename = const (return ())