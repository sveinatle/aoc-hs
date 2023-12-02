module Day04 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn)
import DayProblem
import Debug.Trace
import GHC.Real (reduce)

log2 v = trace (show v) v

cases = [Case solveA "Test" 2, Problem solveA "Problem", Case solveB "Test" 4, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA = length . filter (== True) . map (uncurry hasCompleteOverlap . readPair)

readPair :: String -> ((Int, Int), (Int, Int))
readPair = tuplify . map (map read . splitOn "-") . splitOn ","

tuplify :: [[Int]] -> ((Int, Int), (Int, Int))
tuplify p = ((head (head p), last (head p)), (head (last p), last (last p)))

isScalarInRange :: Int -> (Int, Int) -> Bool
isScalarInRange s r = fst r <= s && s <= snd r

isRangeInRange :: (Int, Int) -> (Int, Int) -> Bool
isRangeInRange r1 r2 = isScalarInRange (fst r1) r2 && isScalarInRange (snd r1) r2

hasCompleteOverlap :: (Int, Int) -> (Int, Int) -> Bool
hasCompleteOverlap a b = isRangeInRange a b || isRangeInRange b a

hasSomeOverlap :: (Int, Int) -> (Int, Int) -> Bool
hasSomeOverlap a b = isScalarInRange (fst a) b || isScalarInRange (snd a) b || isScalarInRange (fst b) a || isScalarInRange (fst b) a

solveB :: [String] -> Int
solveB = length . filter (== True) . map (uncurry hasSomeOverlap . readPair)