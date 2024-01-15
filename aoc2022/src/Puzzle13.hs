module Puzzle13
  ( puzzle13part1
  , puzzle13part2
  , puzzle13part1viz
  , puzzle13part2viz
  ) where

import Data.List.Split
import Data.Char
import Data.List

data DynamicList = I Int | L [DynamicList] deriving(Read)

instance Eq DynamicList where
  (I x) == (I y) = x == y
  (L x) == (L y) = x == y
  (I x) == y = L [I x] == y
  x == (I y) = x == L [I y]

instance Ord DynamicList where
  (I x) <= (I y) = x <= y
  (L a) <= (L b) = a <= b
  x@(I _) <= a = L [x] <= a
  a <= x@(I _) = a <= L [x]

instance Show DynamicList where
  show (I x) = show x
  show (L a) = show a

prep :: String -> String
prep [] = []
prep (c:cs)
  | isDigit c = "I " ++ digits ++ prep rest
  | c == '[' = "L [" ++ prep cs
  | otherwise = c : prep cs
  where (digits, rest) = span isDigit (c:cs)

puzzle13part1 :: String -> String
puzzle13part1 = show . sum . map fst . filter (not . snd) . zip [1..] . map ((\[x, y] -> (x :: DynamicList) <= y) . map ((read :: String -> DynamicList) . prep)) . splitOn [""] . lines

div1, div2 :: DynamicList
div1 = L [L [I 2]]
div2 = L [L [I 6]]

--show . product . map fst . filter ((`elem` [div1, div2]) . snd) . zip [1..]
puzzle13part2 :: String -> String
puzzle13part2 = show . product . map fst . filter ((`elem` [div1, div2]) . snd) . zip [1..] . sort . ([div1, div2] ++) . map ((read :: String -> DynamicList) . prep) . filter (/= "") . lines

-- Visualizations

puzzle13part1viz :: String -> String -> IO ()
puzzle13part1viz filename = const (return ())

puzzle13part2viz :: String -> String -> IO ()
puzzle13part2viz filename = const (return ())