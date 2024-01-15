module Puzzle19
  ( puzzle19part1
  , puzzle19part2
  ) where

import Data.List.Split
import Data.List
import Control.Monad

import Tools

type Point = [Int]

point :: (Int, Int, Int) -> Point
point (x, y, z) = [x, y, z]

psub :: Point -> Point -> Point
psub = zipWith (-)

translate :: Point -> Point -> Point
translate = zipWith (+)

readPoint :: String -> Point
readPoint = ensure3 . map read . splitOn ","
  where ensure3 xs = if length xs /= 3 then undefined else xs

dsqr :: Point -> Point -> Int
dsqr a b = sum $ map sqr $ psub a b
  where sqr x = x * x

deletes :: [(Int, a)] -> [Int] -> [[(Int, a)]]
deletes [] _ = []
deletes xs [] = undefined
deletes xs (y:ys) =
  let remaining = filter ((/= y) . fst) xs
  in remaining : deletes remaining ys

solve :: [[Point]] -> [Point]
solve readings = nub $ concat $ map snd $ normalized
  where readings' = zip [0..] readings
        normalized = head readings' : concat (map (map snd) $ zipWith (flip match) compares (map snd normalized))
        compares = deletes readings' (map fst normalized)

{-cullDups first new = cullDups' [first] new
cullDups' xs [] = xs
cullDups' check (x:xs) =
  if x `elem` check
  then cullDups' check xs
  else cullDups' (x:check) xs-}

orientations :: [Point -> Point]
orientations = do
  -- Look along positive or negative axis
  flip <- [id, negate]
  -- Which axis
  axis <- [ \f [x,y,z] -> f [flip y,z] ++ [flip x]
          , \f [x,y,z] -> f [flip $ negate x,z] ++ [flip y]
          , \f [x,y,z] -> f [flip y,negate x] ++ [flip z]
          ]
  -- How much rotated
  rot <- [ \[x, y] -> [x, y]
         , \[x, y] -> [y, negate x]
         , \[x, y] -> [negate x, negate y]
         , \[x, y] -> [negate y, x]
         ]
  return (axis rot)
  where mid a [b,c] = [b, a, c]

match :: [Point] -> [(Int, [Point])] -> [(Point, (Int, [Point]))]
match readings compares = do
  (index, compare) <- compares
  take 1 $ do
    orientation <- orientations
    anchor <- readings
    let reoriented = map orientation compare
    reference <- drop 11 reoriented
    let reframed = map (translate (psub anchor reference)) reoriented
    let matches = filter (`elem` readings) reframed
    guard (length matches >= 12)
    return (psub anchor reference, (index, reframed))

puzzle19part1 :: String -> String
puzzle19part1 = id -- show . length . solve . map (map readPoint . filter (not . null) . lines) . filter (not . ('s' `elem`)) . filter (not . null) . splitOn "---"

solve' :: [[Point]] -> [Point]
solve' readings = nub $ map fst $ normalized
  where readings' = zip [0..] readings
        normalized = ([0,0,0], head readings') : concat (zipWith (flip match) compares (map (snd . snd) normalized))
        compares = deletes readings' (map (fst . snd) normalized)

puzzle19part2 :: String -> String
puzzle19part2 = show . maximum . map (\(p1, p2) -> sum $ map abs $ psub p1 p2) . uniquePairs . solve' . map (map readPoint . filter (not . null) . lines) . filter (not . ('s' `elem`)) . filter (not . null) . splitOn "---"