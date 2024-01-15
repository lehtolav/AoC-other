module Puzzle8
  ( puzzle8part1
  , puzzle8part2
  ) where

import Text.Regex.TDFA
import Data.List
import qualified Data.Set as S

import Tools

parseReadings :: String -> ([String], [String])
parseReadings line = splitAt 10 $ (getAllTextMatches (line =~ "[a-g]+") :: [String])

puzzle8part1 :: String -> String
puzzle8part1 = show . length . filter (\x -> x == 7 || x <= 4) . concat . map (map length . snd . parseReadings) . lines

ofSize :: [S.Set a] -> Int -> [S.Set a]
ofSize sets size = filter ((== size) . S.size) sets

isOfSize :: S.Set a -> Int -> Bool
isOfSize set size = S.size set == size

deduceDigits :: [String] -> String -> Int
deduceDigits segments = matchNumber numbers . S.fromList
  where sets = map S.fromList segments
        zeroSixNine = sets `ofSize` 6
        twoThreeFive = sets `ofSize` 5
        zero = single $ filter (/= six) $ filter (/= nine) $ zeroSixNine
        one = single $ sets `ofSize` 2
        two = single $ filter ((`isOfSize` 2) . (`S.intersection` four)) twoThreeFive
        three = single $ filter ((`isOfSize` 2) . (`S.intersection` one)) twoThreeFive
        four = single $ sets `ofSize` 4
        five = single $ filter ((`isOfSize` 5) . (`S.intersection` six)) twoThreeFive
        six = single $ filter ((`isOfSize` 1) . (`S.intersection` one)) zeroSixNine
        seven = single $ sets `ofSize` 3
        eight = single $ sets `ofSize` 7
        nine = single $ filter ((`isOfSize` 4) . (`S.intersection` four)) zeroSixNine
        numbers = zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]

matchNumber :: [(S.Set Char, Int)] -> S.Set Char -> Int
matchNumber dict key = match
  where (Just match) = lookup key dict

solveLine :: ([String], [String]) -> Int
solveLine (clues, display) = read . concat . map (show . digits) $ display 
  where digits = deduceDigits clues

puzzle8part2 :: String -> String
puzzle8part2 = show . sum . map (solveLine . parseReadings) . lines