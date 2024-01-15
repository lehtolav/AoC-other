module Puzzle10
  ( puzzle10part1
  , puzzle10part2
  , puzzle10part1viz
  , puzzle10part2viz
  ) where

import Data.List.Split

data Instruction =
    NoOp
  | AddX Int

parseInstruction :: String -> Instruction
parseInstruction "noop" = NoOp
parseInstruction ('a':'d':'d':'x':' ':n) = AddX (read n)

run :: Int -> [Instruction] -> [Int]
run _ [] = []
run x (NoOp:rest) = x : run x rest
run x (AddX v : rest) = x : x : run (x + v) rest

puzzle10part1 :: String -> String
puzzle10part1 = show . sum . map (uncurry (*)) . take 6 . filter ((`elem` [20, 60, 100, 140, 180, 220]) . fst) . zip [1..] . run 1 . map parseInstruction . lines

draw :: Bool -> Char
draw True = '#'
draw False = '.'

puzzle10part2 :: String -> String
puzzle10part2 = unlines . chunksOf 40 . map draw . map (\(x, y) -> abs ((x `mod` 40) - y) < 2) . zip [0..] . run 1 . map parseInstruction . lines

-- Visualizations

puzzle10part1viz :: String -> String -> IO ()
puzzle10part1viz filename = const (return ())

puzzle10part2viz :: String -> String -> IO ()
puzzle10part2viz filename = const (return ())