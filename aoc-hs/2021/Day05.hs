module Day05 where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 5, Problem solveA "Problem", Case solveB "Test" 12, Problem solveB "Problem"]

type Coord = (Int, Int)

solveA :: [String] -> Int
solveA textLines =
  let coords = concatMap (line2coordsStraight . readLine) textLines
      lineCounts = map length (group $ sort coords)
   in length $ filter (>= 2) lineCounts

solveB :: [String] -> Int
solveB textLines =
  let coords = concatMap (line2coordsAll . readLine) textLines
      lineCounts = map length (group $ sort coords)
   in length $ filter (>= 2) lineCounts

readLine :: [Char] -> (Coord, Coord)
readLine textLine =
  case map (splitOn ",") (splitOn " -> " textLine) of
    [[x1, y1], [x2, y2]] -> ((read x1, read y1), (read x2, read y2))
    _ -> error "Invalid input."

line2coordsStraight :: (Coord, Coord) -> [Coord]
line2coordsStraight ((x1, y1), (x2, y2))
  | x1 == x2 = zip (range x1 x2) (range y1 y2)
  | y1 == y2 = zip (range x1 x2) (range y1 y2)
  | otherwise = []

line2coordsAll :: (Coord, Coord) -> [Coord]
line2coordsAll ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range from to
  | from == to = repeat from
  | from < to = [from .. to]
  | from > to = reverse [to .. from]
range _ _ = error "what?"

-- TODO: Use a Map for counting instead of sorting all the coords?