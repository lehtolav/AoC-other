module Puzzle7
  ( puzzle7part1
  , puzzle7part2
  , puzzle7part1viz
  , puzzle7part2viz
  ) where

import Data.List
import Data.Maybe
import Data.Char

data File = Directory String [File] | File String Int deriving(Show)
type Path = [String]
type CommandLine = [String]

build :: CommandLine -> File
build cmd =
  let oracle = build' cmd [] oracle
  in Directory "Root" $ contentsOf [] oracle

build' :: CommandLine -> Path -> [(Path, [File])] -> [(Path, [File])]
build' [] _ _ = []
build' ("$ ls":cmd) path oracle =
  let (listing, cmd') = break ((=='$') . head) cmd
  in buildDir listing path oracle : build' cmd' path oracle
build' ("$ cd ..":cmd) path oracle = build' cmd (tail path) oracle
build' ("$ cd /":cmd) path oracle = build' cmd [] oracle
build' (('$':' ':'c':'d':' ':dir):cmd) path oracle = build' cmd (dir:path) oracle

buildDir :: [String] -> Path -> [(Path, [File])] -> (Path, [File])
buildDir listing path oracle = (path, map (buildFile path oracle) listing)

buildFile :: Path -> [(Path, [File])] -> String -> File
buildFile path oracle file
  | isPrefixOf "dir" file =
    let dir = (drop 4 file)
    in Directory dir (contentsOf (dir:path) oracle)
  | otherwise =
    let (size, filename) = span isDigit file
    in File (tail filename) (read size)

contentsOf :: Path -> [(Path, [File])] -> [File]
contentsOf path = snd . fromJust . find ((==path) . fst)

dirSizes :: File -> ([Int], Int)
dirSizes (File _ size) = ([], size)
dirSizes (Directory _ files) = (fs:ds, fs)
  where sizes = map dirSizes files
        fs = sum $ map snd sizes
        ds = concat $ map fst sizes

puzzle7part1 :: String -> String
puzzle7part1 = show . sum . filter (<=100000) . fst . dirSizes . build . lines

puzzle7part2 :: String -> String
puzzle7part2 input = show . minimum . map fst $ filter ((>=30000000) . snd) $ zip sizes (map (+ free) sizes)
  where root = build $ lines input
        (sizes, total) = dirSizes root
        free = 70000000 - total

-- Visualizations

puzzle7part1viz :: String -> String -> IO ()
puzzle7part1viz filename = const (return ())

puzzle7part2viz :: String -> String -> IO ()
puzzle7part2viz filename = const (return ())