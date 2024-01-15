module Puzzle14
  ( puzzle14part1
  , puzzle14part2
  , puzzle14part1viz
  , puzzle14part2viz
  ) where

import qualified Data.Map as M
import Data.Map (adjust, (!))
import qualified Data.Set as S
import Data.Set (Set, insert, split, splitMember, findMin)
import Data.List.Split hiding(split)
import Data.Maybe
import Data.Either
import Data.List (tails)

import Tools

type Point = (Int, Int)
type Map = M.Map Int (Set Int)

toPair :: [a] -> (a, a)
toPair [a, b] = (a, b)

parseLine :: String -> [Point]
parseLine = concat . map (toLine . toPair . take 2) . init . init . tails . map toPair . map (map read . splitOn ",") . splitOn "->"

toLine :: (Point, Point) -> [Point]
toLine (a, b) =
  if fst a == fst b
  then zip (repeat $ fst a) (fromTo (snd a) (snd b)) -- Vertical
  else zip (fromTo (fst a) (fst b)) (repeat $ snd a) -- Horizontal
  where fromTo a b = if a < b then [a..b] else [b..a]

fromPoints :: [Point] -> Map
fromPoints = M.map S.fromList . M.fromListWith (++) . map (\(x, y) -> (x, [y]))

fallFrom :: Point -> Map -> Either Map Point
fallFrom (x, y) terrain =
  if M.notMember x terrain || S.null below
  then Right (x, y)
  else hitGround (x, findMin below - 1) terrain
  where (_, below) = split y (terrain ! x)

hitGround :: Point -> Map -> Either Map Point
hitGround (x, y) terrain =
  if bleft && bright
  then restAt (x, y) terrain
  else if bleft
       then fallFrom pright terrain
       else fallFrom pleft terrain
  where (pleft, pright) = ((x-1, y+1), (x+1, y+1))
        (bleft, bright) = (blocked pleft terrain, blocked pright terrain)

blocked :: Point -> Map -> Bool
blocked (x, y) terrain = maybe False (S.member y) (M.lookup x terrain)

restAt :: Point -> Map -> Either Map Point
restAt (500, 0) = const (Right (500, 0))
restAt (x, y) = Left . adjust (insert y) x . insNewCol x
  where insNewCol x terrain = if M.notMember x terrain then M.insert x S.empty terrain else terrain

streamSand :: Map -> [Either Map Point]
streamSand terrain = either (\left -> Left left : streamSand left) (\right -> [Right right]) $ fallFrom (500,0) terrain

puzzle14part1 :: String -> String
puzzle14part1 = show . length . lefts . streamSand . fromPoints . concat . map parseLine . lines

fallFrom' :: Int -> Point -> Map -> Either Map Point
fallFrom' maxy (x, y) terrain =
  if M.notMember x terrain || S.null below
  then restAt (x, maxy) terrain
  else hitGround' maxy (x, findMin below - 1) terrain
  where (_, below) = split y (terrain ! x)

hitGround' :: Int -> Point -> Map -> Either Map Point
hitGround' maxy (x, y) terrain =
  if bleft && bright
  then restAt (x, y) terrain
  else if bleft
       then fallFrom' maxy pright terrain
       else fallFrom' maxy pleft terrain
  where (pleft, pright) = ((x-1, y+1), (x+1, y+1))
        (bleft, bright) = (blocked pleft terrain, blocked pright terrain)

streamSand'' :: Int -> Map -> [Either Map Point]
streamSand'' maxy terrain = either (\left -> Left left : streamSand'' maxy left) (\right -> [Right right]) $ fallFrom' maxy (500,0) terrain

streamSand' :: [Point] -> [Either Map Point]
streamSand' terrain = streamSand'' maxy (fromPoints terrain)
  where maxy = (+1) $ maximum $ map snd terrain

puzzle14part2 :: String -> String
puzzle14part2 = show . (+1) . length . lefts . streamSand' . concat . map parseLine . lines

-- Visualizations

puzzle14part1viz :: String -> String -> IO ()
puzzle14part1viz filename = const (return ())

puzzle14part2viz :: String -> String -> IO ()
puzzle14part2viz filename = const (return ())