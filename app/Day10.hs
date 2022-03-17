module Day10 where

import Data.List (find)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 26397, P solveB 288957)

solveA :: [String] -> Int
solveA lines = sum $ map scoreLine lines

solveB :: [String] -> Int
solveB lines = 8

scoreLine = scoreLine' []
  where
    pairs = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
    scores = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
    scoreLine' stack [] = 0
    scoreLine' stack (s : ss) = case find ((s ==) . fst) scores of
      Just (closer, score) ->
        if (head stack, closer) `elem` pairs
          then scoreLine' (tail stack) ss
          else score
      Nothing -> scoreLine' (s : stack) ss

trace' msg value = trace (msg ++ " " ++ show value) value