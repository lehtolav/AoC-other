module Puzzle2
  ( puzzle2part1
  , puzzle2part2
  , puzzle2part1viz
  , puzzle2part2viz
  ) where

import Tools

data Hand = Rock | Paper | Scissors

scoreHand :: Hand -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreMatch :: Hand -> Hand -> Int
scoreMatch elf me = scoreHand me + scoreWin elf me

scoreWin :: Hand -> Hand -> Int
scoreWin Rock Paper = 6
scoreWin Rock Scissors = 0
scoreWin Paper Rock = 0
scoreWin Paper Scissors = 6
scoreWin Scissors Rock = 6
scoreWin Scissors Paper = 0
scoreWin _ _ = 3

parseHand :: Char -> Hand
parseHand 'A' = Rock
parseHand 'B' = Paper
parseHand 'C' = Scissors
parseHand 'X' = Rock
parseHand 'Y' = Paper
parseHand 'Z' = Scissors

parseMatch :: String -> (Hand, Hand)
parseMatch (elf:_:me:_) = (parseHand elf, parseHand me)

puzzle2part1 :: String -> String
puzzle2part1 = show . sum . map (uncurry scoreMatch . parseMatch) . lines

parseMatch' :: String -> (Hand, Hand)
parseMatch' (elf:_:me:_) = let elfHand = parseHand elf in (elfHand, parseHand' elfHand me)

parseHand' :: Hand -> Char -> Hand
parseHand' Rock 'X' = Scissors
parseHand' Paper 'X' = Rock
parseHand' Scissors 'X' = Paper
parseHand' elf 'Y' = elf
parseHand' Rock 'Z' = Paper
parseHand' Paper 'Z' = Scissors
parseHand' Scissors 'Z' = Rock

puzzle2part2 :: String -> String
puzzle2part2 = show . sum . map (uncurry scoreMatch . parseMatch') . lines

-- Visualizations

puzzle2part1viz :: String -> String -> IO ()
puzzle2part1viz filename = const (return ())

puzzle2part2viz :: String -> String -> IO ()
puzzle2part2viz filename = const (return ())