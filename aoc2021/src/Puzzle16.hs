module Puzzle16
  ( puzzle16part1
  , puzzle16part2
  ) where

import Numeric
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split

import Debug.Trace

import Tools

toBits :: Int -> Integer -> [Int]
toBits maxBit n = map (toInt . testBit n) [maxBit, maxBit - 1 .. 0]
  where toInt b = if b then 1 else 0

fromBits = fromBits' . reverse

fromBits' :: [Int] -> Int
fromBits' [] = 0
fromBits' (x:xs) = x + 2 * fromBits' xs

data Packet =
    Literal Int Integer
  | Operator Int Int [Packet] deriving(Show)

readPacket :: [Int] -> (Packet, [Int])
readPacket (v1:v2:v3:t1:t2:t3:bs) =
  if ptype == 4
  then (Literal version literal, lrest)
  else (Operator version ptype subPackets, orest)
  where version = v3 + 2 * v2 + 4 * v1
        ptype = t3 + 2 * t2 + 4 * t1
        (subPackets, orest) = readOperator bs
        (literal, lrest) = readLiteral bs

readLiteral :: [Int] -> (Integer, [Int])
readLiteral bs = (foldl' (\acc x -> acc * 16 + x) 0 numbers, bs')
  where (numbers, bs') = readLiteral' bs

readLiteral' :: [Int] -> ([Integer], [Int])
readLiteral' (c:b1:b2:b3:b4:bs) =
  if c == 1
  then let (xs, bs') = readLiteral' bs
       in (number:xs, bs')
  else ([number], bs)
  where number = fromIntegral $ b4 + 2 * b3 + 4 * b2 + 8 * b1

readOperator :: [Int] -> ([Packet], [Int])
readOperator (0:bs) =
  let nBits = fromBits $ take 15 bs
  in (readAllPackets (take nBits $ drop 15 bs), drop (15 + nBits) bs)
readOperator (1:bs) =
  let nPackets = fromBits $ take 11 bs
  in readNPackets nPackets (drop 11 bs)

readAllPackets :: [Int] -> [Packet]
readAllPackets [] = []
readAllPackets bs = let (packet, rest) = readPacket bs in packet : readAllPackets rest

readNPackets :: Int -> [Int] -> ([Packet], [Int])
readNPackets 0 bs = ([], bs)
readNPackets x bs =
  let (packet, rest) = readPacket bs
      (more, rest') = readNPackets (x - 1) rest
  in (packet:more, rest')

-- Solution to question

addVersions :: Packet -> Int
addVersions (Literal v _) = v
addVersions (Operator v _ ps) = v + sum (map addVersions ps)

puzzle16part1 :: String -> String
puzzle16part1 input = show . addVersions . fst . readPacket . toBits (nChars * 4 - 1) $ inHex input
  where inHex = fst . head . readHex
        nChars = length (filter (not . isSpace) input)

eval :: Packet -> Integer
eval (Literal _ x) = x
eval (Operator _ 0 ps) = sum $ map eval ps
eval (Operator _ 1 ps) = product $ map eval ps
eval (Operator _ 2 ps) = minimum $ map eval ps
eval (Operator _ 3 ps) = maximum $ map eval ps
eval (Operator _ 5 [l,r]) = if eval l > eval r then 1 else 0
eval (Operator _ 6 [l,r]) = if eval l < eval r then 1 else 0
eval (Operator _ 7 [l,r]) = if eval l == eval r then 1 else 0

pretty :: Packet -> String
pretty (Literal _ x) = show x
pretty (Operator _ 0 ps) = "sum(" ++ intercalate "+" (map pretty ps) ++ ")"
pretty (Operator _ 1 ps) = "prd(" ++ intercalate "*" (map pretty ps) ++ ")"
pretty (Operator _ 2 ps) = "min(" ++ intercalate "," (map pretty ps) ++ ")"
pretty (Operator _ 3 ps) = "max(" ++ intercalate "," (map pretty ps) ++ ")"
pretty (Operator _ 5 [l,r]) = "(" ++ pretty l ++ ">" ++ pretty r ++ ")"
pretty (Operator _ 6 [l,r]) = "(" ++ pretty l ++ "<" ++ pretty r ++ ")"
pretty (Operator _ 7 [l,r]) = "(" ++ pretty l ++ "=" ++ pretty r ++ ")"

puzzle16part2 :: String -> String
puzzle16part2 input = show . eval . fst . readPacket . toBits (nChars * 4 - 1) $ inHex input
  where inHex = fst . head . readHex
        nChars = length (filter (not . isSpace) input)