{-# LANGUAGE TupleSections #-}

module Day25 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import qualified Data.Foldable as HashSet
import qualified Data.HashSet as HashSet
import Data.Ix
import Data.List as List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

cases =
  [ CaseStr solveA "Test" "2=-1=0",
    ProblemStr solveA "Problem",
    Case solveB "Test" 999,
    Problem solveB "Problem"
  ]

snafuDigitValue :: Char -> Int
snafuDigitValue = (\x -> x - 2) . fromJust . flip elemIndex ['=', '-', '0', '1', '2']

snafuDigitFromValue :: Int -> Char
snafuDigitFromValue x = ['=', '-', '0', '1', '2'] !! (x + 2)

snafu2dec :: String -> Int
snafu2dec snafu =
  let dec2snafu' [] = (1, 0)
      dec2snafu' (c : cs) =
        let (positionValue, rightValue) = dec2snafu' cs
            value = snafuDigitValue c * positionValue + rightValue
         in (positionValue * 5, value)
   in snd $ dec2snafu' snafu

dec2snafu :: Int -> String
dec2snafu dec =
  let (higherValue, digitCount) = until ((> dec) . fst) (\(value, digitCount) -> (value + 2 * (5 ^ digitCount), digitCount + 1)) (0, 0)
      diff = higherValue - dec
      makeNegativeDiff diff digitPosition digits =
        if digitPosition == 0
          then
            if diff > 4 || diff < 0
              then error "Unexpected diff in ones-position."
              else diff : digits
          else
            let positionValue = 5 ^ digitPosition
                digit = diff `div` positionValue
                positionResult = digit * positionValue
                remaining = diff - positionResult
             in if digit > 4
                  then error "To high digit detected in negative diff."
                  else makeNegativeDiff remaining (digitPosition -1) (digit : digits)
      subtractDigits = makeNegativeDiff diff (digitCount -1) []
   in reverse $ map (snafuDigitFromValue . (-) 2) subtractDigits

solveA :: [String] -> String
solveA = dec2snafu . sum . map snafu2dec

solveB :: [String] -> Int
solveB lines = 0

{-
1
5
25
125
625
3125
15625

target: 4890
max: 7812
diff: 2922

-}