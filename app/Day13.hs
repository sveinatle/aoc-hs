module Day13 where

import Data.List (intercalate, nub, sort, stripPrefix, transpose)
import Data.List.Split (chunk, chunksOf, splitOn)
import Data.Tuple (swap)
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 17, Case solveA "Problem" 0, Case solveB "Test" 0, Case solveB "Test" 0]

data Fold = X Int | Y Int

solveA :: [String] -> Int
solveA lines =
  let (holes, folds) = readProblem lines
   in length $ solve holes (take 1 folds)

readProblem lines =
  let holes = map readHole $ filter (elem ',') lines
      folds = map readFold $ filter (elem '=') lines
   in (holes, folds)

solve :: [(Int, Int)] -> [Fold] -> [(Int, Int)]
solve = foldl foldHoles

readHole :: [Char] -> (Int, Int)
readHole line = case splitOn "," line of
  [x, y] -> (read x, read y)
  _ -> error "Invalid coordinate."

readFold :: [Char] -> Fold
readFold line = case stripPrefix "fold along " line of
  Nothing -> error "Expected prefix."
  Just foldStr -> case splitOn "=" foldStr of
    ["x", n] -> X $ read n
    ["y", n] -> Y $ read n
    _ -> error "Invalid fold."

foldHoles :: [(Int, Int)] -> Fold -> [(Int, Int)]
foldHoles holes fold =
  let foldHole (x, y) = case fold of
        X xFold -> if x > xFold then (xFold - (x - xFold), y) else (x, y)
        Y yFold -> if y > yFold then (x, yFold - (y - yFold)) else (x, y)
   in nub $ map foldHole holes

solveB :: [String] -> Int
solveB lines =
  let (holes, folds) = readProblem lines
      solution = solve holes folds
      drawing = showSolution solution
   in trace drawing 0

showSolution :: [(Int, Int)] -> String
showSolution holes =
  let w = maximum (map fst holes) + 1
      h = maximum (map snd holes) + 1
      canvas = replicate (w * h) ' '
      drawing = foldl (drawHole w) canvas holes
   in intercalate "\n" $ chunksOf w drawing

drawHole :: Int -> [Char] -> (Int, Int) -> [Char]
drawHole w canvas (x, y) =
  let i = w * y + x
   in take i canvas ++ "#" ++ drop (i + 1) canvas
