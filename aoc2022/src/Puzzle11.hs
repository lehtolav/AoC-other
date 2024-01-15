module Puzzle11
  ( puzzle11part1
  , puzzle11part2
  , puzzle11part1viz
  , puzzle11part2viz
  ) where

import Data.Char
import Data.List.Split
import Data.Foldable
import Data.List

import Tools

type Item = Int
type Inspect = (Int -> Int)
type Action = (Int, Int, Int)
data Monke = Monkas [Item] Inspect Action

parseMonke :: [String] -> Monke
parseMonke (_:items:op:test:taction:faction:_) =
  Monkas items' op' (rnum test, rnum taction, rnum faction)
  where items' = reverse . map read . splitOn "," . dropWhile (not . isDigit) $ items
        [arg1, opf, arg2] = words . tail . dropWhile (/= '=') $ op
        getArg x = if x == "old" then id else const (read x)
        f = if opf == "*" then (*) else (+)
        op' x = f (getArg arg1 x) (getArg arg2 x)
        rnum = read . filter isDigit

monkaSee :: Monke -> ([Item], Int, Action)
monkaSee (Monkas items op act) = (reverse items, op 2, act)

monkaDo :: Monke -> [(Int, Item)]
monkaDo (Monkas items insp (test, tm, fm)) = map (act . (`div` 3) . insp) $ items
  where act x = (if x `mod` test == 0 then tm else fm, x)

monkaGib :: Item -> Monke -> Monke
monkaGib item (Monkas items op act) = Monkas (item:items) op act

monkaThrow :: [(Int, Item)] -> IndexedZipperList (Int, Monke) -> IndexedZipperList (Int, Monke)
monkaThrow [] ms = ms
monkaThrow ((to, item):moar) ms = izlModify (\(n, m) -> (n, monkaGib item m)) . izlGoto to $ monkaThrow moar ms

monkaSDo :: Int -> IndexedZipperList (Int, Monke) -> IndexedZipperList (Int, Monke)
monkaSDo turn monkaS = monkaThrow (monkaDo m) $ izlPut (n + length items, (Monkas [] op act)) monkaS'
  where monkaS' = izlGoto turn monkaS
        (n, m@(Monkas items op act)) = izlGet monkaS'

puzzle11part1 :: String -> String
puzzle11part1 = show . product . take 2 . reverse . sort . map fst . fromIzl . (flip $ foldl' (flip monkaSDo)) (concat $ take 20 $ repeat [0..7]) . toIzl . zip (repeat 0) . map parseMonke . chunksOf 7 . lines

-- 9,699,690

monkaDo' :: Monke -> [(Int, Item)]
monkaDo' (Monkas items insp (test, tm, fm)) = map (act . (`mod` 9699690) . insp) $ items
  where act x = (if x `mod` test == 0 then tm else fm, x)

monkaSDo' :: Int -> IndexedZipperList (Int, Monke) -> IndexedZipperList (Int, Monke)
monkaSDo' turn monkaS = monkaThrow (monkaDo' m) $ izlPut (n + length items, (Monkas [] op act)) monkaS'
  where monkaS' = izlGoto turn monkaS
        (n, m@(Monkas items op act)) = izlGet monkaS'

puzzle11part2 :: String -> String
puzzle11part2 = show . product . take 2 . reverse . sort . map fst . fromIzl . (flip $ foldl' (flip monkaSDo')) (concat $ take 10000 $ repeat [0..7]) . toIzl . zip (repeat 0) . map parseMonke . chunksOf 7 . lines

-- Visualizations

puzzle11part1viz :: String -> String -> IO ()
puzzle11part1viz filename = const (return ())

puzzle11part2viz :: String -> String -> IO ()
puzzle11part2viz filename = const (return ())