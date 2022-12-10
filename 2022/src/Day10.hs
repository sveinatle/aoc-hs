{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day10 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import DayProblem (Case (Case, Problem))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 13140,
    Problem solveA "Problem",
    Case solveB "Test" 0,
    Problem solveB "Problem"
  ]

type State a = (Int, Int, [a])

updateMarks :: Int -> Int -> [Int] -> [Int]
updateMarks cycle x marks = if (cycle - 20) `mod` 40 == 0 then cycle * x : marks else marks

thd :: (a, b, c) -> c
thd (_, _, v) = v

process :: State Int -> String -> State Int
process (cycle, x, marks) "noop" = (cycle + 1, x, updateMarks cycle x marks)
process (cycle, x, marks) line =
  let [cmd, valueStr] = splitOn " " line
   in (cycle + 2, x + read valueStr, updateMarks (cycle + 1) x $ updateMarks cycle x marks)

solveA :: [String] -> Int
solveA lines = sum $ thd $ foldl process (1, 1, []) lines

addPixel :: Int -> Int -> [Char] -> [Char]
addPixel cycle spriteX pixels =
  let drawX = (cycle -1) `mod` 40
      p = if drawX - spriteX >= (-1) && drawX - spriteX <= 1 then '#' else '.'
   in p : pixels

render :: State Char -> String -> State Char
render (cycle, x, pixels) "noop" = (cycle + 1, x, addPixel cycle x pixels)
render (cycle, x, pixels) line =
  let [cmd, valueStr] = splitOn " " line
   in (cycle + 2, x + read valueStr, addPixel (cycle + 1) x $ addPixel cycle x pixels)

draw str =
  let lines = chunksOf 40 (reverse str)
   in t0 $ map t0 lines

solveB :: [String] -> Int
solveB lines = draw $ thd $ foldl render (1, 1, []) lines