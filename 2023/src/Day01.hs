module Day01 where

import Data.Char (digitToInt, isDigit)
import Data.List
import Data.Maybe (isJust)
import DayProblem
import Debug.Trace

log v = trace (show v) v

cases = [Case solveA "Test" 142, Problem solveA "Problem", Case solveB "Test2" 281, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA = sum . map readCalibration

readCalibration :: String -> Int
readCalibration line = 10 * firstDigit line + (firstDigit . reverse) line

firstDigit :: String -> Int
firstDigit line = case find isDigit line of
  Just d -> digitToInt d
  Nothing -> error "No digit found."

solveB :: [String] -> Int
solveB = sum . map readAlphaCalibration

readAlphaCalibration :: String -> Int
readAlphaCalibration line =
  let candidates = tails line
      first = find isJust $ map tryReadAlphaDigit candidates
      last = find isJust $ map tryReadAlphaDigit (reverse candidates)
   in case (first, last) of -- Could be improved with Monad join?
        (Just (Just first), Just (Just last)) -> 10 * first + last
        _ -> error "No digit found."

tryReadAlphaDigit :: String -> Maybe Int
tryReadAlphaDigit text =
  let idx = elemIndex True $ map (`isPrefixOf` text) ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
   in case idx of
        Just idx -> Just $ if idx <= 8 then idx + 1 else idx - 9
        Nothing -> Nothing