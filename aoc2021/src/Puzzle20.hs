module Puzzle20
  ( puzzle20part1
  , puzzle20part2
  ) where

import qualified Data.Map as M

import Tools
{-
cojoin :: IndexedZipperList a -> IndexedZipperList (IndexedZipperList a)
cojoin izl = (IZL (izlIndex izl) lefts izl rights)
  where lefts = tail $ iterate izlLeft izl
        rights = tail $ iterate izlRight izl



extend :: IndexedZipperList a -> (IndexedZipperList a -> b) -> IndexedZipperList b
extend izl f = izlMap f (cojoin izl)

rule :: M.Map Int Bool -> IndexedZipperList (IndexedZipperList Bool) -> Bool
rule rules (IZL _ (l:_) c (r:_)) = M.lookup $ bitListToInt (context l ++ context c ++ context r)
  where context (IZL _ (a:_) b (c:_)) = [a, b, c]-}

puzzle20part1 :: String -> String
puzzle20part1 = id

puzzle20part2 :: String -> String
puzzle20part2 = id