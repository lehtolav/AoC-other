module Tools where

-- Various shortcuts to be used in the solutions

import Control.Comonad
import Control.Monad
import Data.List.NonEmpty (fromList, toList)
import Data.List

import Debug.Trace

-- Use comonadic extend on a non-(non-empty) list
-- The list must be non-empty
extend' :: ([a] -> b) -> [a] -> [b]
extend' f = toList . extend (f . toList) . fromList

-- Read to Int
readInt :: String -> Int
readInt = read

intToBit :: Int -> Bool
intToBit 0 = False
intToBit 1 = True
intToBit _ = undefined

bitToInt :: Bool -> Int
bitToInt True = 1
bitToInt False = 0

-- List of bools to integer
bitListToInt :: [Bool] -> Int
bitListToInt = bitListToInt' . map bitToInt . reverse

bitListToInt' :: [Int] -> Int
bitListToInt' [] = 0
bitListToInt' (x:xs) = x + 2 * bitListToInt' xs

-- Return a single value from list or fail
single :: Show a =>  [a] -> a
single [x] = x
single y = error ("not single: " ++ show y)

-- Return windows of n size into the list
windows :: Int -> [a] -> [[a]]
windows n = filter ((== n) . length) . map (take n) . tails

data IndexedZipperList a = IZL Int [a] a [a]

toIzl :: [a] -> IndexedZipperList a
toIzl (x:xs) = IZL 0 [] x xs
toIzl _ = error "Empty list"

fromIzl :: IndexedZipperList a -> [a]
fromIzl (IZL _ ls c rs) = reverse ls ++ (c:rs)

izlLeft :: IndexedZipperList a -> IndexedZipperList a
izlLeft (IZL 0 _ _ _) = error "Can't left anymore"
izlLeft (IZL i (l:ls) c rs) = IZL (i - 1) ls l (c:rs)

izlRight :: IndexedZipperList a -> IndexedZipperList a
izlRight (IZL _ _ _ []) = error "Can't right anymore"
izlRight (IZL i ls c (r:rs)) = IZL (i + 1) (c:ls) r rs

izlCanLeft :: IndexedZipperList a -> Bool
izlCanLeft (IZL _ ls _ _) = not $ null ls

izlCanRight :: IndexedZipperList a -> Bool
izlCanRight (IZL _ _ _ rs) = not $ null rs

izlGotoRel :: Int -> IndexedZipperList a -> IndexedZipperList a
izlGotoRel i =
  if i >= 0
  then (!! i) . iterate izlRight
  else (!! (negate i)) . iterate izlLeft

izlGoto :: Int -> IndexedZipperList a -> IndexedZipperList a
izlGoto n izl@(IZL i _ _ _) = izlGotoRel (n - i) izl

izlUp :: IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izlUp outer@(IZL _ _ (IZL i _ _ _) _) = izlModify (izlGoto i) $ izlLeft outer

izlDown :: IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izlDown outer@(IZL _ _ (IZL i _ _ _) _) = izlModify (izlGoto i) $ izlRight outer

izlGet :: IndexedZipperList a -> a
izlGet (IZL _ _ c _) = c

izlPut :: a -> IndexedZipperList a -> IndexedZipperList a
izlPut c (IZL i ls _ rs) = IZL i ls c rs

izlModify :: (a -> a) -> IndexedZipperList a -> IndexedZipperList a
izlModify f izl = izlPut (f (izlGet izl)) izl

izlCoord :: IndexedZipperList (IndexedZipperList a) -> (Int, Int)
izlCoord (IZL y _ (IZL x _ _ _) _) = (x, y)

izl2dGoto :: (Int, Int) -> IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izl2dGoto (x, y) = izlModify (izlGoto x) . izlGoto y

izlOnLeft :: (IndexedZipperList a -> IndexedZipperList a) -> IndexedZipperList a -> IndexedZipperList a
izlOnLeft f izl@(IZL i _ _ _) =
    if izlCanLeft izl
    then izlGoto i $ f (izlLeft izl)
    else izl

izlOnRight :: (IndexedZipperList a -> IndexedZipperList a) -> IndexedZipperList a -> IndexedZipperList a
izlOnRight f izl@(IZL i _ _ _) =
    if izlCanRight izl
    then izlGoto i $ f (izlRight izl)
    else izl

izl2dOnLeft :: (IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)) -> IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izl2dOnLeft f izl =
    if izlCanLeft (izlGet izl)
    then izl2dGoto (x, y) $ f (izlModify izlLeft izl)
    else izl
    where (x, y) = izlCoord izl

izl2dOnRight :: (IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)) -> IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izl2dOnRight f izl =
    if izlCanRight (izlGet izl)
    then izl2dGoto (x, y) $ f (izlModify izlRight izl)
    else izl
    where (x, y) = izlCoord izl

izlOnUp :: (IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)) -> IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izlOnUp f izl =
    if izlCanLeft izl
    then izl2dGoto (x, y) $ f (izlUp izl)
    else izl
    where (x, y) = izlCoord izl

izlOnDown :: (IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)) -> IndexedZipperList (IndexedZipperList a) -> IndexedZipperList (IndexedZipperList a)
izlOnDown f izl =
    if izlCanRight izl
    then izl2dGoto (x, y) $ f (izlDown izl)
    else izl
    where (x, y) = izlCoord izl

izlMap :: (a -> b) -> IndexedZipperList a -> IndexedZipperList b
izlMap f (IZL i ls c rs) = IZL i (map f ls) (f c) (map f rs)

izlCat :: [a] -> IndexedZipperList a -> IndexedZipperList a
izlCat [] (IZL i ls _ rs) = (IZL i ls (head rs) (tail rs))
izlCat list (IZL i ls _ rs) = (IZL i ls (head list) ((tail list) ++ rs))

izlIndex :: IndexedZipperList a -> Int
izlIndex (IZL i _ _ _) = i

uniquePairs :: [a] -> [(a,a)]
uniquePairs xs = do
  x <- tails xs
  guard (not $ null x)
  let (y:ys) = x
  map (y,) ys

traceit x = trace (show x) x