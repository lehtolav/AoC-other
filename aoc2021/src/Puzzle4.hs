module Puzzle4
  ( puzzle4part1
  , puzzle4part2
  ) where

import Data.List.Split
import Data.Function
import Data.List
import Data.Ord

import Tools

-- Winning turn, score
type BingoBoardWin = (Int, Int)

parseDraw :: String -> [Int]
parseDraw = map read . splitOn ","

rowsAndColumns :: [[a]] -> [[a]]
rowsAndColumns board = board ++ map (\x -> map (!! x) board) [0..(length board - 1)]

-- Winning turn, Winning number
findWinner :: [Int] -> [Int] -> (Int, Int)
findWinner draw line = (turn, draw !! turn)
  where (Just w) = findIndex (`elem` line) (reverse draw)
        turn = (length draw - 1) - w

bingoScore :: [Int] -> [[Int]] -> Int -> Int
bingoScore draw board number = number * sum unmarked
  where called = number : takeWhile (/= number) draw
        unmarked = filter (not . (`elem` called)) $ concat board

solveBingo :: [Int] -> [[Int]] -> BingoBoardWin
solveBingo draw board = (fst fastest, bingoScore draw board $ snd fastest)
  where rAndC = rowsAndColumns board
        winners = map (findWinner draw) rAndC
        fastest = minimumBy (compare `on` fst) winners

puzzle4part1 :: String -> String
puzzle4part1 input = show . snd . minimumBy (compare `on` fst) . map (solveBingo draw) $ boards
  where inputLines = lines input
        draw = parseDraw $ head inputLines
        boards = chunksOf 5 $ map (map readInt . words) $ filter ((> 0) . length) $ tail inputLines

-- Copypasta
puzzle4part2 :: String -> String
puzzle4part2 input = show . snd . maximumBy (compare `on` fst) . map (solveBingo draw) $ boards
  where inputLines = lines input
        draw = parseDraw $ head inputLines
        boards = chunksOf 5 $ map (map readInt . words) $ filter ((> 0) . length) $ tail inputLines
