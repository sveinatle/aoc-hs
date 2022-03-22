module Day10 where

import Data.Either (fromLeft)
import Data.List (elemIndex, find, sort)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 26397, Case solveA "Problem" 0, Case solveB "Test" 288957, Case solveB "Problem" 0]

solveA :: [String] -> Int
solveA lines = sum $ map (fromLeft 0 . checkLine) lines

solveB :: [String] -> Int
solveB lines =
  let results = map checkLine lines
      scores = mapMaybe computeScore results
   in sort scores !! (length scores `div` 2)

checkLine :: [Char] -> Either Int [Char]
checkLine = checkLine' []
  where
    pairs = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
    scores = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
    checkLine' stack [] = Right stack
    checkLine' stack (s : ss) = case find ((s ==) . fst) scores of
      Just (closer, score) ->
        if (head stack, closer) `elem` pairs
          then checkLine' (tail stack) ss
          else Left score
      Nothing -> checkLine' (s : stack) ss

computeScore :: Either Int [Char] -> Maybe Int
computeScore (Left invalid) = Nothing
computeScore (Right stack) = Just $ foldl (\score sym -> score * 5 + symScore sym) 0 stack
  where
    scores = ['(', '[', '{', '<']
    symScore sym = (+) 1 $ fromJust $ elemIndex sym scores

trace' msg value = trace (msg ++ " " ++ show value) value