module Day12 where

import Data.Char (isLower, isUpper)
import Data.Either (fromLeft)
import Data.List (delete, elemIndex, elemIndices, find, group, sort, (\\))
import Data.List.Split
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 226, P solveB 3509)

solveA :: [String] -> Int
solveA lines =
  let ((t1Expected, _), t1Lines) = test1
      ((t2Expected, _), t2Lines) = test2
      t1Result = trace' "Test 1" $ solve t1Lines False
      t2Result = solve t2Lines False
   in if t1Expected /= t1Result
        then error $ "Test 1 failed. Result = " ++ show t1Result
        else
          if t2Expected /= t2Result
            then error $ "Test 2 failed. Result = " ++ show t2Result
            else solve lines False

solveB :: [String] -> Int
solveB lines =
  let ((_, t1Expected), t1Lines) = test1
      ((_, t2Expected), t2Lines) = test2
      t1Result = trace' "Test 1" $ solve t1Lines True
      t2Result = solve t2Lines True
   in if t1Expected /= t1Result
        then error $ "Test 1 failed. Result = " ++ show t1Result
        else
          if t2Expected /= t2Result
            then error $ "Test 2 failed. Result = " ++ show t2Result
            else solve lines True

trace' msg value = trace (msg ++ " " ++ show value) value

trace'' f value = trace (f value) value

start = "start"

end = "end"

solve lines allowTwice =
  let connections = filter isValidDirection $ concatMap ((\[a, b] -> [(a, b), (b, a)]) . splitOn "-") lines
      includesLower (a, b) = isLower (head a) || isLower (head b)
      paths = visit connections allowTwice start []
      paths' = trace'' showPaths paths
   in length paths
  where
    isValidDirection (from, to) = from /= end && to /= start

showPaths :: [[String]] -> String
showPaths paths = unlines $ map showPath paths
  where
    showPath path = unwords (start : reverse path)

visit :: [(String, String)] -> Bool -> String -> [String] -> [[String]]
visit [] _ path _ = []
visit connections allowTwice currentCave path
  | currentCave == end = [path]
  | not allowTwice && (not . null . countSmallRevisits) path = []
  | allowTwice && (sum . countSmallRevisits) path > 2 = []
  | otherwise =
    let connectionsNext = filter ((== currentCave) . fst) connections
        visitNext connection =
          let cave' = snd connection
              path' = (cave' : path)
           in visit connections allowTwice cave' path'
     in concatMap visitNext connectionsNext
  where
    countSmallRevisits path =
      let smallCaves = sort $ filter (isLower . head) path
          cavesMoreThanOnce = filter (> 1) $ map length $ group smallCaves
       in cavesMoreThanOnce

test1 =
  ( (10, 36),
    [ "start-A",
      "start-b",
      "A-c",
      "A-b",
      "b-d",
      "A-end",
      "b-end"
    ]
  )

test2 =
  ( (19, 103),
    [ "dc-end",
      "HN-start",
      "start-kj",
      "dc-start",
      "dc-HN",
      "LN-dc",
      "HN-end",
      "kj-sa",
      "kj-HN",
      "kj-dc"
    ]
  )