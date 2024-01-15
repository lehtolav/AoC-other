module Puzzle5
  ( puzzle5part1
  , puzzle5part2
  , puzzle5part1viz
  , puzzle5part2viz
  ) where

import Data.Char
import Data.List
import Data.List.Split

import Tools

type Instruction = (Int, Int, Int)
type Stacks = IndexedZipperList String
type Model = Stacks -> Instruction -> Stacks

parseStacks :: [String] -> [String]
parseStacks = map (filter isAlpha) . transpose . map (map head . chunksOf 4 . tail)

parseInstruction :: String -> Instruction
parseInstruction line = (read amount, read from - 1, read to - 1)
 where (amount, r1) = span isDigit $ drop 5 line
       (from, r2) = span isDigit $ drop 6 r1
       to = drop 4 r2

crane :: Model -> [String] -> [Instruction] -> [String]
crane model stacks instr = crane' model (toIzl stacks) instr

crane' :: Model -> Stacks -> [Instruction] -> [String]
crane' _ stacks [] = fromIzl stacks
crane' model stacks (i:is) = crane' model (model stacks i) is

pick :: Int -> String -> (String, String)
pick n stack = (reverse pickd, stack')
  where (pickd, stack') = splitAt n stack

crateMover9000 :: Model
crateMover9000 stack (amount, from, to) = izlModify ((++) pickd) $ izlGoto to $ izlPut left stack'
  where stack' = izlGoto from stack
        (pickd, left) = pick amount $ izlGet stack'

puzzle5part1 :: String -> String
puzzle5part1 = map head . uncurry (crane crateMover9000) . (\(stacks, insts) -> (parseStacks stacks, map parseInstruction (drop 2 insts))) . break (any isDigit) . lines

pick' :: Int -> String -> (String, String)
pick' n stack = (pickd, stack')
  where (pickd, stack') = splitAt n stack

crateMover9001 :: Model
crateMover9001 stack (amount, from, to) = izlModify ((++) pickd) $ izlGoto to $ izlPut left stack'
  where stack' = izlGoto from stack
        (pickd, left) = pick' amount $ izlGet stack'

puzzle5part2 :: String -> String
puzzle5part2 = map head . uncurry (crane crateMover9001) . (\(stacks, insts) -> (parseStacks stacks, map parseInstruction (drop 2 insts))) . break (any isDigit) . lines

-- Visualizations

puzzle5part1viz :: String -> String -> IO ()
puzzle5part1viz filename = const (return ())

puzzle5part2viz :: String -> String -> IO ()
puzzle5part2viz filename = const (return ())