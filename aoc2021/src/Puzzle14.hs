module Puzzle14
  ( puzzle14part1
  , puzzle14part2
  ) where

import qualified Data.Map as M
import Data.Char
import Data.List

import Tools

type Pair = (Char, Char)
type Rules = M.Map Pair Char

parseInput :: [String] -> (String, Rules)
parseInput (polymer:lines) = (polymer, M.fromList $ map (toRule . filter isAlpha) $ lines)
  where toRule [a, b, c] = ((a, b), c)

insertPair :: Rules -> String -> String
insertPair rules polymer = head polymer : (concat . map applyRules . filter ((== 2) . length) . map (take 2) . tails) polymer
  where applyRules [a, b] =
          case M.lookup (a, b) rules of
            Just c -> [c, b]
            Nothing -> [b]

puzzle14part1 :: String -> String
puzzle14part1 input = show . solution . counts $ (iterate (insertPair rules) $ polymer) !! 10
  where (polymer, rules) = (parseInput . filter (not . null) . lines) input
        counts = map snd . M.toList . M.fromListWith (+) . map (,1)
        solution cs = maximum cs - minimum cs

-- As expected, something better is needed

type Amounts = M.Map Pair Int

updateAmounts :: Rules -> Amounts -> Amounts
updateAmounts rules = M.fromListWith (+) . concat . map polymerize . M.toList
  where polymerize (pair, amount) =
          case M.lookup pair rules of
            Just c -> [((fst pair, c), amount), ((c, snd pair), amount)]
            Nothing -> [(pair, amount)]

polymerAmounts :: String -> Amounts
polymerAmounts polymer = M.fromListWith (+) . map (,1) $ zip polymer (tail polymer)

puzzle14part2 :: String -> String
puzzle14part2 input = show . solution . counts $ (iterate (updateAmounts rules) $ polymerAmounts polymer) !! 40
  where (polymer, rules) = (parseInput . filter (not . null) . lines) input
        first = head polymer
        counts = map snd . M.toList . M.fromListWith (+) . map (\(a, b) -> (snd a, b)) . M.toList . M.insertWith (+) (first, first) 1
        solution cs = maximum cs - minimum cs