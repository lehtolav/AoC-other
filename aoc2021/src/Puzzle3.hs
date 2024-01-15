module Puzzle3
  ( puzzle3part1
  , puzzle3part2
  ) where

import Data.Maybe
import Data.Bits (xor)

import Tools

nBits :: [[Int]] -> [Bool]
nBits inputs = map (>= (half + mod)) $ foldr (zipWith (+)) (repeat 0) inputs
  where (half, mod) = length inputs `divMod` 2

puzzle3part1 :: String -> String
puzzle3part1 input = show $ toNumber bits * toNumber (map not bits)
  where inputs :: [[Int]]
        inputs = map (map (read . return)) $ lines input
        bits = nBits inputs
        toNumber = bitListToInt

refilter :: (Bool -> Bool) -> [[Int]] -> [Bool]
refilter f inputs = refilter' f 0 (map f $ nBits inputs) inputs

refilter' :: (Bool -> Bool) -> Int -> [Bool] -> [[Int]] -> [Bool]
refilter' _ _ _ [x] = map intToBit x
refilter' _ _ _ []  = error "lol"
refilter' f a b xs  = refilter' f (a + 1) (map f $ nBits ys) ys
  where compareTo = bitToInt $ b !! a
        ys = filter ((== compareTo) . (!! a)) xs


puzzle3part2 :: String -> String
puzzle3part2 input = show $ oxygen * scrubber
  where inputs :: [[Int]]
        inputs = map (map (read . return)) $ lines input
        oxygen = bitListToInt $ refilter id inputs
        scrubber = bitListToInt $ refilter not inputs