module Puzzle17
  ( puzzle17part1
  , puzzle17part2
  ) where

import Tools

-- x=102..157, y=-146..-90

triangle :: Int -> Int
triangle x = x * (x + 1) `div` 2

puzzle17part1 :: String -> String
puzzle17part1 = show . triangle . const 145

-- t = x(x + 1) / 2
-- 2t = x^2 + x
-- x = (-1 +- sqrt (1 + 8t)) / 2

invTriangle :: Int -> Double
invTriangle x = ((sqrt $ 1 + 8 * fromIntegral x) - 1) / 2

-- x = v0t + at^2
-- at^2 + v0t - x = 0, a = -1

xTime :: Int -> Int -> Int
xTime x v =
  if determinant < 0
  then 1000 -- In this case, this means we never reach it, so "a lot of time"
  else ceiling $ maximum $ map (\x -> ((sqrt $ x) - fromIntegral v) / 2) [determinant, -determinant]
  where determinant = fromIntegral $ (v*v) - 4 * x

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (x1, x2) (y1, y2) = max (-1) (min x2 y2 - max x1 y1) >= 0

puzzle17part2 :: String -> String
puzzle17part2 _ = show . length $ do
  vx <- [14..157]
  let minXTime = xTime 102 vx
  let maxXTime = xTime 157 vx
  vy <- [-146.. -90] ++ [-72..145]
  let minYTime = xTime (negate 90) vy
  let maxYTime = xTime (negate 146) vy
  if fuckingSimulateIGuess vx vy --overlap (minXTime, maxXTime) (minYTime, maxYTime)
  then return (vx, vy)
  else []

fuckingSimulateIGuess :: Int -> Int -> Bool
fuckingSimulateIGuess vx vy = fuckingSimulateIGuess' 0 0 vx vy

fuckingSimulateIGuess' :: Int -> Int -> Int -> Int -> Bool
fuckingSimulateIGuess' x y vx vy =
  if x >= 102 && x <= 157 && y >= -146 && y <= -90
  then True
  else if x > 157 || y < -146
       then False
       else fuckingSimulateIGuess' (x + vx) (y + vy) (dampen vx) (vy - 1)
  where dampen x = if x > 0 then x - 1 else 0