{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day11 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

data Operation = Add Int | Mul Int | Sqr deriving (Show)

data Monkey = Monkey {mId :: Int, mItems :: [Int], mOp :: Operation, mDivisor :: Int, mNext :: (Int, Int), mInvestigations :: Int} deriving (Show)

giveItem :: Monkey -> Int -> Monkey
giveItem (Monkey id items op divisor next ig) item = Monkey id (items ++ [item]) op divisor next ig

finishRound :: Monkey -> Monkey
finishRound (Monkey id items op divisor next ig) = Monkey id [] op divisor next (ig + length items)

type Relief = Int -> Int

cases =
  [ Case solveA "Test" 10605,
    Problem solveA "Problem",
    Case solveB "Test" 2713310158,
    Problem solveB "Problem"
  ]

readMonkey :: [String] -> Monkey
readMonkey [idStr, itemsStr, opStr, divisorStr, throw1Str, throw2Str] =
  let id = (read . init . last . words) idStr
      items = (map read . splitOn ", " . last . splitOn ": ") itemsStr
      op = case (words . last . splitOn " = ") opStr of
        ["old", "*", "old"] -> Sqr
        ["old", "+", v] -> Add (read v)
        ["old", "*", v] -> Mul (read v)
        _ -> error "Unexpected pattern in operation."
      divisor = (read . last . words) divisorStr
      throw1 = (read . last . words) throw1Str
      throw2 = (read . last . words) throw2Str
   in Monkey id items op divisor (throw1, throw2) 0
readMonkey lines = trace (show lines) error "Unexpected input lines"

readMonkeys :: [String] -> [Monkey]
readMonkeys lines = map readMonkey (splitOn [""] lines)

investigateItem :: Relief -> Monkey -> Int -> (Int, Int)
investigateItem relief (Monkey id items op divisor (throwTrue, throwFalse) ig) item =
  let newItem =
        relief $ case op of
          Add a -> item + a
          Mul a -> item * a
          Sqr -> item * item
      decision = (newItem `mod` divisor) == 0
      toMonkey = if decision then throwTrue else throwFalse
   in (newItem, toMonkey)

playItem :: Relief -> Monkey -> [Monkey] -> Int -> [Monkey]
playItem relief monkey monkeys item =
  let (newItem, toMonkeyId) = investigateItem relief monkey item
      toMonkey = monkeys !! toMonkeyId
   in take toMonkeyId monkeys ++ giveItem toMonkey newItem : drop (toMonkeyId + 1) monkeys

playMonkey :: Relief -> [Monkey] -> Int -> [Monkey]
playMonkey relief monkeys monkeyId =
  let monkey = monkeys !! monkeyId
      monkeys' = foldl (playItem relief monkey) monkeys (mItems monkey)
   in take monkeyId monkeys' ++ finishRound monkey : drop (monkeyId + 1) monkeys'

playRound :: Relief -> [Monkey] -> [Monkey]
playRound relief monkeys = foldl (playMonkey relief) monkeys [0 .. length monkeys - 1]

solve :: Int -> Relief -> [Monkey] -> Int
solve rounds relief =
  product
    . last2
    . sort
    . map mInvestigations
    . (!! rounds)
    . iterate (playRound relief)
  where
    last2 list = drop (length list - 2) list

solveA :: [String] -> Int
solveA = solve 20 (`div` 3) . readMonkeys

solveB :: [String] -> Int
solveB lines =
  let monkeys = readMonkeys lines
      commonDiv = product $ map mDivisor monkeys
   in solve 10000 (`mod` commonDiv) monkeys
