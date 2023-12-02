module Day09 where

import Data.Char (digitToInt)
import Data.List (elemIndices, intersperse, sort, transpose)
import Data.List.Split (chunksOf)
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 15, Problem solveA "Problem", Case solveB "Test" 1134, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines =
  let rows = map (map digitToInt) lines
      columns = transpose rows
      rowBoolsFlattened = concatMap findMinimums rows
      columnBoolsFlattened = concat $ transpose $ map findMinimums columns
      intersectedBoolsFlattened = zipWith (&&) rowBoolsFlattened columnBoolsFlattened
      rowsFlattened = concat rows
      minimumValues = map snd $ filter fst (zip intersectedBoolsFlattened rowsFlattened)
   in risk minimumValues

risk :: [Int] -> Int
risk points = sum $ map (+ 1) points

findMinimums :: [Int] -> [Bool]
findMinimums values =
  let pairs = zip values (tail values)
      smallest1 = map (uncurry (<)) pairs ++ [True]
      smallest2 = True : map (uncurry (>)) pairs
   in zipWith (&&) smallest1 smallest2

solveB :: [String] -> Int
solveB lines =
  let rows = map (map digitToInt) lines
      columns = transpose rows
      rowBoolsFlattened = concatMap findMinimums rows
      columnBoolsFlattened = concat $transpose $ map findMinimums columns
      intersectedBoolsFlattened = zipWith (&&) rowBoolsFlattened columnBoolsFlattened
      width = length (head rows)
      indexToCoord i = (i `mod` width, i `div` width)
      basinCoords = map indexToCoord $ elemIndices True intersectedBoolsFlattened
      basins = getBasins rows basinCoords []
   in product $ take 3 $ reverse $ sort $ map length basins

getBasinCells :: [[Int]] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getBasinCells rows x y cells =
  if isBasin rows x y && notElem (x, y) cells
    then
      getBasinCells rows x (y -1) $
        getBasinCells rows x (y + 1) $
          getBasinCells rows (x -1) y $
            getBasinCells rows (x + 1) y $
              (x, y) : cells
    else cells
  where
    isBasin rows x y =
      x >= 0
        && y >= 0
        && x < length (head rows)
        && y < length rows
        && rows !! y !! x < 9

getBasins rows [] basins = basins
getBasins rows ((x, y) : basinCoords) basins =
  if any ((x, y) `elem`) basins
    then getBasins rows basinCoords basins -- Skip coord if it's already in another basin.
    else getBasins rows basinCoords (getBasinCells rows x y [] : basins) -- Else find basin cells for this coord and add to basin list.

trace' msg value = trace (msg ++ " " ++ show value) value
