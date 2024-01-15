module Puzzle18
  ( puzzle18part1
  , puzzle18part2
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

import Tools

{-

data SnailNumber = Number Int | Pair (SnailNumber, SnailNumber)

type Stack = SnailNumber -> SnailNumber
type Zipper = (SnailNumber, [Stack], [(Stack, Bool)])

push :: Zipper -> Zipper
push (n, ss, ps) = (n, id:ss, ps)

pop :: Zipper -> Zipper
pop (n, [], ps) = error "Nothing to pop"
pop (n, s:ss, ps) = (s n, ss, ps)

isPair :: Zipper -> Bool
isPair (Pair _ _, _, _) = True
isPair _ = False

dl :: Zipper -> Zipper
dl (n, [], ps) = dl (n, [id], ps)
dl (Pair l r, s:ss, ps) = (l, (s . (\x -> Pair x r)):ss, (\x -> (Pair x r, False)):ps)
dl _ = error "Cannot descent into a number"

dr :: Zipper -> Zipper
dr (n, [], ps) = dr (n, [id], ps)
dr (Pair l r, s:ss, ps) = (r, (s . (\x -> Pair l x)):ss, (\x -> (Pair l x, True)):ps)
dr _ = error "Cannot descent into a number"

hasParent :: Zipper -> Bool
hasParent (_, _, []) = False
hasParent _ = True

parent :: Zipper -> (Zipper, Bool)
parent (n, [], ps) = parent (n, [id], ps)
parent (n, _, []) = error "No parent"
parent (n, s:ss, p:ps) = (((fst p) n, (s . fst p):ss, ps), snd p)

parentUntil :: Bool -> Zipper -> Zipper
parentUntil right z =
  let (p, r) = parent z
  in if r == right then p else parentUntil right p

downUntilNumber :: Bool -> Zipper -> Zipper
downUntilNumber d z = if isPair z then downUntilNumber d (dir z) else z
  where dir = if d then dr else dl

insert :: SnailNumber -> Zipper -> Zipper
insert n (_, ss, ps) = (n, ss, ps)

nextToLeft :: Zipper -> Zipper
nextToLeft = downUntilNumber True . dl . parentUntil True

nextToRight :: Zipper -> Zipper
nextToRight = downUntilNumber False . dr . parentUntil False

-}

data Token = Push | Pop | Number Int deriving(Show, Eq)

type SFN = IndexedZipperList Token

pretty :: [Token] -> String
pretty [] = []
pretty (Push:xs) = '[' : pretty xs
pretty (Pop:xs)  = ']' : pretty xs
pretty (Number x:xs) = (show x) ++ pretty xs

parseNumber :: String -> [Token]
parseNumber [] = []
parseNumber (',':xs) = parseNumber xs
parseNumber ('[':xs) = Push : parseNumber xs
parseNumber (']':xs) = Pop  : parseNumber xs
parseNumber xs =
  let (nums, rest) = span isNumber xs
  in (Number $ read nums) : parseNumber rest

tisNumber :: Token -> Bool
tisNumber (Number _) = True
tisNumber _ = False

add :: Int -> Token -> Token
add x (Number y) = Number (x + y)
add _ _ = error "NaN"

rewind = toIzl . fromIzl

explode :: Int -> SFN -> SFN
explode d n
  | izlCanRight n =
    case (izlGet n, izlGet (izlRight n)) of
      (Push, _) -> explode (d + 1) (izlRight n)
      (Pop,  _) -> explode (d - 1) (izlRight n)
      (Number x, Number y) ->
        if d > 4
        then explode (d - 1) . izlPut (Number 0) . (!! 3) . iterate (izlCat []) . izlLeft $ explode' n x y
        else explode d (izlRight n) 
      _ -> explode d (izlRight n)
  | otherwise = split $ rewind n

explode' :: SFN -> Int -> Int -> SFN
explode' n a b = izlGoto start . (\n' -> addToNumber n' b $ rights n') . izlGoto start . addToNumber n a $ lefts n
  where start = izlIndex n
        lefts = tail . takeWhile izlCanLeft . iterate izlLeft
        rights = drop 2 . takeWhile izlCanRight . iterate izlRight
        addToNumber n x = maybe n (izlModify (add x)) . find (tisNumber . izlGet)

split :: SFN -> SFN
split n
  | izlCanRight n =
    case izlGet n of
      Number x ->
        if x > 9
        then split' n
        else split (izlRight n)
      _ -> split (izlRight n)
  | otherwise = n

split' :: SFN -> SFN
split' n = explode 0 . rewind $ izlCat [Push, Number d, Number (d + m), Pop] n
  where (Number x) = izlGet n
        (d, m) = x `divMod` 2

sfnAdd :: [Token] -> [Token] -> [Token]
sfnAdd [] y = y
sfnAdd x [] = x
sfnAdd x y = fromIzl . explode 0 . toIzl $ Push : (x ++ y ++ [Pop])

magnitude :: [Token] -> Int
magnitude [Number x] = x
magnitude xs = magnitude (magnitude' xs)

magnitude' :: [Token] -> [Token]
magnitude' (Push:Number x:Number y:Pop:rest) = Number (3 * x + 2 * y) : magnitude' rest
magnitude' (x:xs) = x : magnitude' xs
magnitude' [] = []

puzzle18part1 :: String -> String
puzzle18part1 = show . magnitude . foldl' sfnAdd [] . map parseNumber . lines

swaps :: [(a, a)] -> [(a, a)]
swaps xs = do
  (x, y) <- xs
  [(x, y), (y, x)]

puzzle18part2 :: String -> String
puzzle18part2 = show . maximum . map (magnitude . uncurry sfnAdd) . swaps . uniquePairs . map parseNumber . lines