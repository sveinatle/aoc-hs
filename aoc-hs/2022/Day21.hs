{-# LANGUAGE TupleSections #-}

module Day21 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import qualified Data.Foldable as HashSet
import qualified Data.HashSet as HashSet
import Data.Ix
import Data.List as List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
import qualified Data.Map as Map
import Data.Maybe (isJust)
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
  [ Case solveA "Test" 152,
    Problem solveA "Problem",
    Case solveB "Test" 301,
    Problem solveB "Problem"
  ]

data Monkey = Constant Int | Mul String String | Div String String | Add String String | Sub String String deriving (Show)

ar2tup :: [b] -> (b, b)
ar2tup [a, b] = (a, b)
ar2tup _ = error "Unexpected argument to ar2tup."

readOperation :: String -> Monkey
readOperation opStr
  | '*' `elem` opStr = uncurry Mul $ ar2tup $ splitOn " * " opStr
  | '/' `elem` opStr = uncurry Div $ ar2tup $ splitOn " / " opStr
  | '+' `elem` opStr = uncurry Add $ ar2tup $ splitOn " + " opStr
  | '-' `elem` opStr = uncurry Sub $ ar2tup $ splitOn " - " opStr
  | otherwise = Constant (read opStr)

readMonkeys :: [String] -> Map.Map String Monkey
readMonkeys lines = Map.fromList (map ((\[monkeyName, monkeyOpStr] -> (monkeyName, readOperation monkeyOpStr)) . splitOn ": ") lines)

getMonkeyValue :: Map.Map String Monkey -> String -> Int
getMonkeyValue monkeys monkeyName = case monkeys Map.! monkeyName of
  Constant v -> v
  Mul m1 m2 -> getMonkeyValue monkeys m1 * getMonkeyValue monkeys m2
  Div m1 m2 -> getMonkeyValue monkeys m1 `div` getMonkeyValue monkeys m2
  Add m1 m2 -> getMonkeyValue monkeys m1 + getMonkeyValue monkeys m2
  Sub m1 m2 -> getMonkeyValue monkeys m1 - getMonkeyValue monkeys m2

getNonHumanValue :: Map.Map String Monkey -> String -> Maybe Int
getNonHumanValue monkeys monkeyName =
  if monkeyName == "humn"
    then Nothing
    else case monkeys Map.! monkeyName of
      Constant v -> Just v
      Mul m1 m2 -> maybeOp monkeys m1 m2 (*)
      Div m1 m2 -> maybeOp monkeys m1 m2 div
      Add m1 m2 -> maybeOp monkeys m1 m2 (+)
      Sub m1 m2 -> maybeOp monkeys m1 m2 (-)

maybeMonkeys :: Map.Map String Monkey -> String -> String -> (Maybe Int, Maybe Int)
maybeMonkeys monkeys m1 m2 = (getNonHumanValue monkeys m1, getNonHumanValue monkeys m2)

maybeOp :: Map.Map String Monkey -> String -> String -> (Int -> Int -> a) -> Maybe a
maybeOp monkeys m1 m2 op = case maybeMonkeys monkeys m1 m2 of
  (Just m1value, Just m2value) -> Just (op m1value m2value)
  _ -> Nothing

solveA :: [String] -> Int
solveA lines =
  let monkeys = readMonkeys lines
   in getMonkeyValue monkeys "root"

solveB :: [String] -> Int
solveB lines =
  let monkeys = readMonkeys lines
      (left, right) = case monkeys Map.! "root" of
        Constant v -> error "Expected pair of monkeys."
        Mul m1 m2 -> (m1, m2)
        Div m1 m2 -> (m1, m2)
        Add m1 m2 -> (m1, m2)
        Sub m1 m2 -> (m1, m2)
      leftValue = getNonHumanValue monkeys left
      rightValue = getNonHumanValue monkeys right
   in case leftValue of
        Just value -> findHumanValue monkeys value right
        Nothing -> case rightValue of
          Just value -> findHumanValue monkeys value left
          Nothing -> error "Human in both sides?"

-- NOTE: This could probably be improved. It will evaluate the same subtrees many times as it "digs" downward looking for the human.
findHumanValue :: Map.Map String Monkey -> Int -> String -> Int
findHumanValue monkeys resultValue monkeyName =
  if monkeyName == "humn"
    then resultValue
    else case monkeys Map.! monkeyName of
      Constant v -> error "Did not expect constant value while looking for human."
      Mul leftMonkey rightMonkey -> case maybeMonkeys monkeys leftMonkey rightMonkey of
        (Just leftValue, Nothing) -> findHumanValue monkeys (resultValue `div` leftValue) rightMonkey
        (Nothing, Just rightValue) -> findHumanValue monkeys (resultValue `div` rightValue) leftMonkey
        _ -> error "Unexpected state."
      Div leftMonkey rightMonkey -> case maybeMonkeys monkeys leftMonkey rightMonkey of
        (Just leftValue, Nothing) -> findHumanValue monkeys (leftValue `div` resultValue) rightMonkey
        (Nothing, Just rightValue) -> findHumanValue monkeys (resultValue * rightValue) leftMonkey
        _ -> error "Unexpected state."
      Add leftMonkey rightMonkey -> case maybeMonkeys monkeys leftMonkey rightMonkey of
        (Just leftValue, Nothing) -> findHumanValue monkeys (resultValue - leftValue) rightMonkey
        (Nothing, Just rightValue) -> findHumanValue monkeys (resultValue - rightValue) leftMonkey
        _ -> error "Unexpected state."
      Sub leftMonkey rightMonkey -> case maybeMonkeys monkeys leftMonkey rightMonkey of
        (Just leftValue, Nothing) -> findHumanValue monkeys (leftValue - resultValue) rightMonkey
        (Nothing, Just rightValue) -> findHumanValue monkeys (resultValue + rightValue) leftMonkey
        _ -> error "Unexpected state."

--
