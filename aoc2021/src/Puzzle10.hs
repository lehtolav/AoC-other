module Puzzle10
  ( puzzle10part1
  , puzzle10part2
  ) where

import Data.List

import Tools

-- Parens, Bracket, Curly, Angle
data Parens = P | B | C | A deriving(Eq, Show)

data ParseStack =
    Fine [Parens]
  | Illegal Parens
  | UnExpectedEnd

pushParens :: [Parens] -> Char -> [Parens]
pushParens ps '(' = P:ps
pushParens ps '[' = B:ps
pushParens ps '{' = C:ps
pushParens ps '<' = A:ps

popParens :: [Parens] -> Char -> ParseStack
popParens [] _ = UnExpectedEnd
popParens (p:ps) ')' = if p == P then Fine ps else Illegal P
popParens (p:ps) ']' = if p == B then Fine ps else Illegal B
popParens (p:ps) '}' = if p == C then Fine ps else Illegal C
popParens (p:ps) '>' = if p == A then Fine ps else Illegal A

parseChunk :: ParseStack -> String -> ParseStack
parseChunk ilgl@(Illegal x) _ = ilgl
parseChunk ilgl@(UnExpectedEnd) _ = ilgl
parseChunk x [] = x
parseChunk (Fine stack) (c:cs) =
  if c `elem` "([{<"
  then parseChunk (Fine (pushParens stack c)) cs
  else parseChunk (popParens stack c) cs

scoreIllegal :: ParseStack -> Int
scoreIllegal (Illegal P) = 3
scoreIllegal (Illegal B) = 57
scoreIllegal (Illegal C) = 1197
scoreIllegal (Illegal A) = 25137
scoreIllegal _ = 0

puzzle10part1 :: String -> String
puzzle10part1 = show . sum . map (scoreIllegal . parseChunk (Fine [])) . lines

addition :: Parens -> Int
addition P = 1
addition B = 2
addition C = 3
addition A = 4

scoreIncomplete :: Int -> ParseStack -> Int
scoreIncomplete total (Fine (c:cs)) = scoreIncomplete (total * 5 + addition c) (Fine cs)
scoreIncomplete total _ = total

takeMiddle :: [a] -> a
takeMiddle list = list !! (length list `div` 2)

puzzle10part2 :: String -> String
puzzle10part2 = show . takeMiddle . sort . filter (>0) . map (scoreIncomplete 0 . parseChunk (Fine [])) . lines