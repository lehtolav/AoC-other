module Puzzle1
  ( puzzle1part1
  , puzzle1part2
  , puzzle1part1viz
  , puzzle1part2viz
  ) where

import Data.Maybe
import Data.List
import Data.List.Split
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Export

puzzle1part1 :: String -> String
puzzle1part1 = show . maximum . map (sum . map read) . splitOn [""] . lines

puzzle1part2 :: String -> String
puzzle1part2 = show . sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines

-- Visualizations

makeBar toth w h = translate 0 (-(toth-h)/2) $ pictures [color red $ rectangleSolid w h, color black $ rectangleWire w h] -- , rotate 90 $ color white $ text (show $ floor h)
tlate (x, pic) = translate x 0 pic

puzzle1part1viz :: String -> String -> IO ()
puzzle1part1viz filename input = do
  let barw = 20
  let toth = 100
  let barwf = fromIntegral barw
  let tothf = fromIntegral toth
  let elves = (reverse . sort . map (sum . map read) . splitOn [""] . lines) input
  let totw = length elves * barw
  let totwf = fromIntegral totw
  let highest = maximum elves
  let pic = pictures (map tlate $ zip (map (\x -> (x+0.5)*barwf - totwf/2) [0..]) $ map (makeBar tothf barwf . (*tothf) . (/highest)) elves)
  exportPictureToPNG (totw, toth) white filename pic

puzzle1part2viz :: String -> String -> IO ()
puzzle1part2viz filename = const (return ())