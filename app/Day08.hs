module Day08 where

import Data.List (delete, elemIndex, find, group, nub, sort, (\\))
import Data.List.Split (splitOn)
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 26, Case solveA "Problem" 0, Case solveB "Test" 61229, Case solveB "Problem" 0]

solveA :: [String] -> Int
solveA lines =
  let displays = parseDisplays lines
      fours = map snd displays
      lengths = map length $concat fours
   in length $filter (`elem` [2, 4, 3, 7]) lengths

parseDisplays :: [String] -> [([String], [String])]
parseDisplays = map parseDisplay

parseDisplay :: [Char] -> ([String], [String])
parseDisplay line =
  let [ten, four] = map words (splitOn "|" line)
   in (ten, four)

solveB :: [String] -> Int
solveB lines =
  let displays = parseDisplays lines
      displayValues = map readDisplay' displays
   in sum displayValues

readDisplay :: ([String], [String]) -> Int
readDisplay display =
  let digits = fst display
      one = findDigitPinsByLength digits 2
      four = findDigitPinsByLength digits 4
      seven = findDigitPinsByLength digits 3
      eight = findDigitPinsByLength digits 7
      segmentPins =
        map
          nub
          [ seven ++ eight,
            four ++ eight,
            one ++ four ++ seven ++ eight,
            four ++ eight,
            eight,
            one ++ four ++ seven ++ eight,
            eight
          ]
      segmentsDigits =
        [ [0, 2, 3, 5, 6, 7, 8, 9],
          [0, 4, 5, 6, 8, 9],
          [0, 1, 2, 3, 4, 7, 8, 9],
          [2, 3, 4, 5, 6, 8, 9],
          [0, 2, 6, 8],
          [0, 1, 3, 4, 5, 6, 7, 8, 9],
          [0, 3, 5, 6, 8, 9]
        ]
   in 9

readDisplay' :: ([String], [String]) -> Int
readDisplay' display =
  let digits = fst display
      pinsWithDigitCount = map (\pins -> (head pins, length pins)) (group $sort $concat digits)
      one = sort $ findDigitPinsByLength digits 2
      four = sort $ findDigitPinsByLength digits 4
      seven = sort $ findDigitPinsByLength digits 3
      eight = sort $ findDigitPinsByLength digits 7
      s0 = single $ seven \\ one
      s1 = single $ findPinsByDigitCount pinsWithDigitCount (length (segmentsDigits !! 1))
      s2 = single $ delete s5 one
      s3 = single $ four \\ (s1 : one)
      s4 = single $ findPinsByDigitCount pinsWithDigitCount (length (segmentsDigits !! 4))
      s5 = single $ findPinsByDigitCount pinsWithDigitCount (length (segmentsDigits !! 5))
      s6 = single $ eight \\ (s0 : s4 : four)
      segmentsDigits =
        [ [0, 2, 3, 5, 6, 7, 8, 9],
          [0, 4, 5, 6, 8, 9],
          [0, 1, 2, 3, 4, 7, 8, 9],
          [2, 3, 4, 5, 6, 8, 9],
          [0, 2, 6, 8],
          [0, 1, 3, 4, 5, 6, 7, 8, 9],
          [0, 2, 3, 5, 6, 8, 9]
        ]
      digitsPinsOrdered = getDigitsPins segmentsDigits [s0, s1, s2, s3, s4, s5, s6]
      pins2digit pins = case elemIndex pins digitsPinsOrdered of
        Just digit -> digit
        Nothing -> error "Digit for pins not found."
      valuePins = map sort $ snd display
      valueDigits = map pins2digit valuePins
   in digitsToValue valueDigits
  where
    single [pin] = pin
    single _ = error "Expected exactly one pin in list."

findDigitPinsByLength :: [String] -> Int -> [Char]
findDigitPinsByLength digits segments = case find (\s -> length s == segments) digits of
  Just n -> n
  Nothing -> error (show segments) ++ " not found"

findPinsByDigitCount :: [(Char, Int)] -> Int -> [Char]
findPinsByDigitCount pinsWithDigitCount digitCount = map fst $ filter (\p -> snd p == digitCount) pinsWithDigitCount

getDigitsPins :: [[Int]] -> [Char] -> [[Char]]
getDigitsPins segmentsDigits segmentPins = map (getDigitPins segmentsDigits segmentPins) [0 .. 9]

getDigitPins :: [[Int]] -> [Char] -> Int -> [Char]
getDigitPins segmentsDigits segmentPins digit = sort $ map snd $ filter (elem digit . fst) $ zip segmentsDigits segmentPins

digitsToValue :: [Int] -> Int
digitsToValue = digitsToValue' 0
  where
    digitsToValue' v [] = v
    digitsToValue' v (d : ds) = digitsToValue' (v * 10 + d) ds

trace' msg value = trace (msg ++ ": " ++ show value) value