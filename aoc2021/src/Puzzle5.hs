module Puzzle5
  ( puzzle5part1
  , puzzle5part2
  ) where

import Text.Regex.TDFA
import qualified Data.Map as M
import Data.List
import Data.Function

import Tools

type Point = (Int, Int)
type Line = (Point, Point)

isHOrV :: Line -> Bool
isHOrV ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

parseLine :: String -> Line
parseLine line = ((x1, y1), (x2, y2))
  where [x1, y1, x2, y2] = map read (getAllTextMatches (line =~ "[0-9]+") :: [String])

fromTo :: Int -> Int -> [Int]
fromTo a b = [min a b .. max a b]

lineCover :: Line -> M.Map Point Int
lineCover line@((x1, y1), (x2, y2)) =
  if isHOrV line
  then M.fromList $ map (,1) points
  else diagonalCover line
  where points = if x1 == x2
                 then map (x1,) $ fromTo y1 y2
                 else map (,y1) $ fromTo x1 x2

diagonalCover :: Line -> M.Map Point Int
diagonalCover line = M.fromList $ map (,1) $ zip (fromTo (fst left) (fst right)) (upDn $ fromTo (snd left) (snd right))
  where [left, right] = sortBy (compare `on` fst) $ [fst line, snd line]
        upDn = if snd right > snd left then id else reverse

puzzle5part1 :: String -> String
puzzle5part1 = show . length . filter (>1) . map snd . M.toList . foldl' (M.unionWith (+)) M.empty . map lineCover . filter isHOrV . map parseLine . filter ((> 0) . length) . lines

puzzle5part2 :: String -> String
puzzle5part2 = show . length . filter (>1) . map snd . M.toList . foldl' (M.unionWith (+)) M.empty . map lineCover . map parseLine . filter ((> 0) . length) . lines