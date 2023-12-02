module Day01 where

import Data.Char (digitToInt, isDigit, isNumber)
import Data.List
import Data.Maybe (isJust)
import DayProblem
import Debug.Trace

cases = [Case solveA "Test" 142, Problem solveA "Problem", Case solveB "Test2" 281, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA = sum . map (readCalibration True)

solveB :: [String] -> Int
solveB = sum . map (readCalibration False)

readCalibration :: Bool -> String -> Int
readCalibration digitsOnly line =
  let candidates = tails line
      first = find isJust $ map (tryReadDigitPrefix digitsOnly) candidates
      last = find isJust $ map (tryReadDigitPrefix digitsOnly) (reverse candidates)
   in case (first, last) of -- Could be improved with Monad join?
        (Just (Just first), Just (Just last)) -> 10 * first + last
        _ -> error "No digit found."

tryReadDigitPrefix :: Bool -> String -> Maybe Int
tryReadDigitPrefix digitsOnly text =
  let terms = (if digitsOnly then take 10 else id) ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      idx = elemIndex True $ map (`isPrefixOf` text) terms
   in case idx of
        Just idx -> Just $ if idx <= 9 then idx else idx - 9
        Nothing -> Nothing