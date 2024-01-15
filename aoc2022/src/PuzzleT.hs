module PuzzleT
  ( puzzleNpart1
  , puzzleNpart2
  , puzzleNpart1viz
  , puzzleNpart2viz
  ) where

import Tools

puzzleNpart1 :: String -> String
puzzleNpart1 = id

puzzleNpart2 :: String -> String
puzzleNpart2 = id

-- Visualizations

puzzleNpart1viz :: String -> String -> IO ()
puzzleNpart1viz filename = const (return ())

puzzleNpart2viz :: String -> String -> IO ()
puzzleNpart2viz filename = const (return ())