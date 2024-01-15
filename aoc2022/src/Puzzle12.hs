module Puzzle12
  ( puzzle12part1
  , puzzle12part2
  , puzzle12part1viz
  , puzzle12part2viz
  ) where

import Data.PSQueue hiding(foldr)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (partition)

import Tools
import Debug.Trace (trace)

type Point = (Int, Int)

parseChar :: Char -> Int
parseChar 'E' = parseChar 'z'
parseChar 'S' = parseChar 'a'
parseChar c = fromEnum c - fromEnum 'a'

parseMap :: String -> (Point, Point, [(Point, Int)])
parseMap themap = (start, end, map (\(p, c) -> (p, parseChar c)) coords)
  where coords = concat . map (\(y, line) -> zipWith (\x c -> ((x, y), c)) [0..] line) . zip [0..] . lines $ themap
        find c = fst . head . filter ((==c) . snd)
        start = find 'S' coords
        end = find 'E' coords

aStar :: (Point, Point, [(Point, Int)]) -> [Point]
aStar (start, end, points) = aStar' heights (fromList [start :-> 0]) (Map.singleton start []) (end, heights Map.! end)
  where heights = (Map.fromList points)

aStar' :: Map.Map Point Int -> PSQ Point Int -> Map.Map Point [Point] -> (Point, Int) -> [Point]
aStar' heights open paths goal =
  if next == fst goal
  then currentPath
  else aStar' heights open'' paths'' goal
  where (next :-> _, open') = fromJust $ minView open
        (nbs', closed) = partition (not . inMap paths . fst) $ neighbors heights next
        faster = map fst . filter ((> currentLength) . length . snd) $ zipMap ((paths Map.!) . fst) closed
        nbs = nbs' ++ faster
        currentPath = paths Map.! next
        currentLength = length currentPath + 1
        paths' = foldr Map.delete paths $ map fst faster
        paths'' = foldr (addPath next) paths' (map fst nbs)
        open'' = foldr (uncurry insert) open' . map (\(x, y) -> (fst x, y)) $ zipMap (heuristic currentLength goal) nbs

addPath :: Point -> Point -> Map.Map Point [Point] -> Map.Map Point [Point]
addPath from to paths = Map.insert to (from : paths Map.! from) paths

diff :: Num a => a -> a -> a
diff a b = abs (a - b)

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = map (\x -> (x, f x))

inMap :: Ord a => Map.Map a b -> a -> Bool
inMap = flip Map.member

neighbors :: Map.Map Point Int -> Point -> [(Point, Int)]
neighbors heights (x, y) = filter ((<=1) . ((flip (-)) height) . snd) . zipMap ((Map.!) heights) . filter (inMap heights) $ npoints
  where npoints = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        height = heights Map.! (x, y)

heuristic :: Int -> (Point, Int) -> (Point, Int) -> Int
heuristic curlen ((gx, gy), gh) ((x, y), h) = diff gx x + diff gy y + curlen

puzzle12part1 :: String -> String
puzzle12part1 = show . length . aStar . parseMap

fromStart :: (Point, Point, [(Point, Int)]) -> Point -> [Point]
fromStart (_, end, heights) start = aStar (start, end, heights)

puzzle12part2 :: String -> String
puzzle12part2 = id -- show . minimum . map length . (\x -> map (fromStart x) (zip (repeat 0) [0..40])) . parseMap

-- Visualizations

puzzle12part1viz :: String -> String -> IO ()
puzzle12part1viz filename = const (return ())

puzzle12part2viz :: String -> String -> IO ()
puzzle12part2viz filename = const (return ())