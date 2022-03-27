{-# LANGUAGE TupleSections #-}

module Day18 where

import Data.Char (digitToInt, isNumber)
import Data.Complex (magnitude)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)
import Numeric (readHex)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

cases =
  [ Case solveA "Test" 4140,
    Problem solveA "Problem",
    Case solveB "Test" 3993,
    Problem solveB "Problem"
  ]

solveA :: [String] -> Int
solveA lines =
  let numbers = map (normalize . fst . parse) lines
      added = foldl1 add numbers
   in mag added

solveB :: [String] -> Int
solveB lines =
  let numbers = map (normalize . fst . parse) lines
      pairs = concat [[(a, b), (b, a)] | a <- numbers, b <- numbers]
   in maximum (map (mag . uncurry add) pairs)

trace' :: Show a => a -> a
trace' value = trace (show value) value

trace'' :: Show a => String -> a -> a
trace'' msg value = trace (msg ++ " " ++ show value) value

add :: Node -> Node -> Node
add n1 n2 = normalize (Pair n1 n2)

data Node = Num Int | Pair Node Node

instance Show Node where
  show (Num v) = show v
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

takeNum :: Node -> Int
takeNum (Num value) = value
takeNum _ = error "Expected Num."

addToLeft :: Node -> Int -> Node
addToLeft (Num n) value = Num (n + value)
addToLeft (Pair left right) value = Pair (addToLeft left value) right

addToRight :: Node -> Int -> Node
addToRight (Num n) value = Num (n + value)
addToRight (Pair left right) value = Pair left $ addToRight right value

parse :: [Char] -> (Node, [Char])
parse ('[' : rest) =
  let (left, afterLeft) = parse rest
      (right, afterRight) = case afterLeft of
        (',' : afterComma) -> parse afterComma
        _ -> error "Expected comma."
      afterClose = case afterRight of
        (']' : afterClose) -> afterClose
        _ -> error "Expect closing bracket."
   in (Pair left right, afterClose)
parse rest =
  let numStr = takeWhile isNumber rest
      afterNum = drop (length numStr) rest
   in (Num $read numStr, afterNum)

data NormalizeExplodeResult = Explosion Node Int Int deriving (Show)

data NormalizeSplitResult = Split Node deriving (Show)

normalize :: Node -> Node
normalize node = case normalizeExplode node 0 of
  Just (Explosion explodedNode _ _) -> normalize explodedNode
  Nothing -> case normalizeSplit node of
    Just (Split splitNode) -> normalize splitNode
    Nothing -> node

normalizeExplode :: Node -> Int -> Maybe NormalizeExplodeResult
normalizeExplode (Pair left right) level =
  if level == 4
    then
      let leftValue = takeNum left
          rightValue = takeNum right
       in Just $ Explosion (Num 0) leftValue rightValue
    else
      let leftResult = normalizeExplode left (level + 1)
          rightResult = normalizeExplode right (level + 1)
       in case leftResult of
            Just (Explosion newLeft explLeft explRight) -> Just $ Explosion (Pair newLeft (addToLeft right explRight)) explLeft 0
            -- Only change right if left was not changed
            Nothing -> case rightResult of
              Just (Explosion newRight explLeft explRight) -> Just $ Explosion (Pair (addToRight left explLeft) newRight) 0 explRight
              Nothing -> Nothing
normalizeExplode (Num n) _ = Nothing

normalizeSplit :: Node -> Maybe NormalizeSplitResult
normalizeSplit (Pair left right) =
  let leftResult = normalizeSplit left
      rightResult = normalizeSplit right
   in case leftResult of
        Just (Split newLeft) -> Just (Split (Pair newLeft right))
        -- Only change right if left was not changed
        Nothing -> case rightResult of
          Just (Split newRight) -> Just (Split (Pair left newRight))
          Nothing -> Nothing
normalizeSplit (Num n) =
  if n >= 10
    then Just $ Split (Pair (Num $ n `div` 2) (Num $n - n `div` 2))
    else Nothing

mag :: Node -> Int
mag (Num v) = v
mag (Pair l r) = 3 * mag l + 2 * mag r