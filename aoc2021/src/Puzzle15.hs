module Puzzle15
  ( puzzle15part1
  , puzzle15part2
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Function
import Data.List

import Tools

-- Let's Dijkstra, sigh

type Point = (Int, Int)

dijkstra :: M.Map Point Int -> Point -> Point -> Int
dijkstra nodes start end = dijkstra' nodes S.empty (opens nodes S.empty (start, 0)) end

dijkstra' :: M.Map Point Int -> S.Set Point -> M.Map Point Int -> Point -> Int
dijkstra' nodes closed open goal =
  if fst next == goal
  then snd next
  else dijkstra' nodes (S.insert (fst next) closed) (M.union (M.delete (fst next) open) $ opens nodes closed next) goal
  where next = minimumBy (compare `on` snd) $ M.toList open

opens :: M.Map Point Int -> S.Set Point -> (Point, Int) -> M.Map Point Int
opens nodes closed ((x, y), cost) = M.intersectionWith (+) nodes potential
  where north = (x, y - 1)
        east = (x + 1, y)
        south = (x, y + 1)
        west = (x - 1, y)
        potential = M.fromList . map (, cost) . S.toList $ S.difference (S.fromList [north, east, south, west]) closed

parseMap :: [String] -> M.Map Point Int
parseMap lines = M.fromList . concat . map (\(y, line) -> map (\(x, char) -> ((x, y), read [char])) $ zip [0..] line) $ zip [0..] lines

puzzle15part1 :: String -> String
puzzle15part1 input = show $ dijkstra nodes (0, 0) (w - 1, h - 1)
  where w = length (head inputLines)
        h = length inputLines
        inputLines = lines input
        nodes = parseMap inputLines

duplicateMap :: Int -> Int -> [(Point, Int)] -> [(Point, Int)]
duplicateMap w h nodes = do
  x <- [0..4]
  y <- [0..4]
  map (addDanger (x + y) . addX (x * w) . addY (y * h)) nodes
  where addDanger dng' (pt, dng) = (pt, ((dng - 1 + dng') `mod` 9) + 1)
        addX x' ((x, y), dng) = ((x + x', y), dng)
        addY y' ((x, y), dng) = ((x, y + y'), dng)

puzzle15part2 :: String -> String
puzzle15part2 input = input -- show $ dijkstra nodes (0, 0) (w * 5 - 1, h * 5 - 1)
  where w = length (head inputLines)
        h = length inputLines
        inputLines = lines input
        nodes = M.fromList . duplicateMap w h . M.toList $ parseMap inputLines