module Puzzle13
  ( puzzle13part1
  , puzzle13part2
  ) where

import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Char

import Tools

data Along = X | Y deriving(Show)
type Point = (Int, Int)
type Fold = (Along, Int)

parsePoint :: String -> Point
parsePoint line = (read x, read y)
  where [x, y] = splitOn "," line

parseInstruction :: String -> Fold
parseInstruction line = (along, read $ tail rest)
  where along = if 'x' `elem` line then X else Y
        (_, rest) = span (/= '=') line

parseAll :: [String] -> (S.Set Point, [Fold])
parseAll lines = (S.fromList $ map parsePoint pointLines, map parseInstruction $ tail foldLines)
  where (pointLines, foldLines) = span (not . null) lines

foldAlong :: Fold -> Point -> Point
foldAlong (X, coord) (x, y) = if x > coord then (2 * coord - x, y) else (x, y)
foldAlong (Y, coord) (x, y) = if y > coord then (x, 2 * coord - y) else (x, y)

doFold :: Fold -> S.Set Point -> S.Set Point
doFold fold = S.fromList . map (foldAlong fold) . S.toList

puzzle13part1 :: String -> String
puzzle13part1 = show . length . S.toList . oneFold . parseAll . lines
  where oneFold (points, folds) = doFold (head folds) points

minmax :: [Int] -> [Int]
minmax xs = [minimum xs .. maximum xs]

buildWallOfText :: S.Set Point -> [String]
buildWallOfText points = map buildLine ys
  where xs = minmax (map fst $ S.toList points)
        ys = minmax (map snd $ S.toList points)
        buildLine y = map (\x -> if S.member (x, y) points then '#' else '.') xs

puzzle13part2 :: String -> String
puzzle13part2 = unlines . buildWallOfText . allFolds . parseAll . lines
  where allFolds (points, folds) = foldl' (flip doFold) points folds