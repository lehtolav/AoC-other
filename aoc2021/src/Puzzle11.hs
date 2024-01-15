module Puzzle11
  ( puzzle11part1
  , puzzle11part2
  ) where

import Data.List

import Tools

type Grid = IndexedZipperList (IndexedZipperList Int)

doFlashes :: Grid -> Grid
doFlashes = izlMap (izlMap (\x -> if x > 9 then 0 else x)) . countEnergy

countEnergy :: Grid -> Grid
countEnergy g = countEnergy' $ izl2dGoto (0, 0) g

countEnergy' :: Grid -> Grid
countEnergy' g =
  if izlCanRight row
  then countEnergy' $ izlModify izlRight ng
  else if izlCanRight ng
       then countEnergy' $ izlModify (izlGoto 0) $ izlRight ng
       else ng
  where row = izlGet ng
        ng = increaseEnergy g

increaseEnergy :: Grid -> Grid
increaseEnergy g =
  if eNow == 9
  then izl2dGoto (x, y) $ flash ng
  else ng
  where (x, y) = izlCoord g
        eNow = izlGet $ izlGet g
        ng = izlModify (izlModify succ) g

flash :: Grid -> Grid
flash = izlOnDown (flashLeftRight . increaseEnergy) . izlOnUp (flashLeftRight . increaseEnergy) . flashLeftRight
  where flashLeftRight = izl2dOnRight increaseEnergy . izl2dOnLeft increaseEnergy

puzzle11part1 :: String -> String
puzzle11part1 = show . (!! 101) . scanl (\sum step -> sum + length (filter (== 0) step)) 0 . map (concat . map fromIzl . fromIzl) . iterate doFlashes . toIzl . map (toIzl . map (read . return)) . lines

puzzle11part2 :: String -> String
puzzle11part2 = show . head . findIndices (all (== 0)) . map (concat . map fromIzl . fromIzl) . iterate doFlashes . toIzl . map (toIzl . map (read . return)) . lines