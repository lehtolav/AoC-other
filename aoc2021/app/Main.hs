module Main where

import System.IO hiding (readFile)
--import System.IO.Strict (readFile)
import System.Directory

import Puzzles

main :: IO ()
main = do
  mapM_ runPuzzle (zip [0..] allPuzzles)

runPuzzle :: (Int, (String -> String)) -> IO ()
runPuzzle (index, solution) = do
  let (day, part) = index `divMod` 2
  input <- do
    exists <- doesFileExist $ inputName day part
    if exists
    then readFile' (inputName day part)
    else readFile' (inputName day (1 - part))
  writeFile (outputName day part) (solution input)

-- day 1 part "a" is index 0, so even numbers are "a" parts
partName :: Int -> String
partName 0 = "a"
partName 1 = "b"

inputName :: Int -> Int -> String
inputName day part = "inputs/" ++ show (day + 1) ++ partName part

outputName :: Int -> Int -> String
outputName day part = "outputs/" ++ show (day + 1) ++ partName part

readFile' name = openFile name ReadMode >>= hGetContents