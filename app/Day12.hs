module Day12 where

import Data.Char (isUpper)
import Data.Either (fromLeft)
import Data.List (delete, elemIndex, elemIndices, find, sort, (\\))
import Data.List.Split
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 226, P solveB 3509)

solveA :: [String] -> Int
solveA lines =
  let (t1Expected, t1Lines) = test1
      (t2Expected, t2Lines) = test2
      t1Result = trace' "Test 1" $ solveA' t1Lines
      t2Result = solveA' t2Lines
   in if t1Expected /= t1Result
        then error $ "Test 1 failed. Result = " ++ show t1Result
        else
          if t2Expected /= t2Result
            then error $ "Test 2 failed. Result = " ++ show t2Result
            else solveA' lines

solveB :: [String] -> Int
solveB lines = 4

trace' msg value = trace (msg ++ " " ++ show value) value

trace'' f value = trace (f value) value

start = "start"

end = "end"

solveA' lines =
  let connections = filter isValidDirection $ concatMap ((\[a, b] -> [(a, b), (b, a)]) . splitOn "-") lines
      paths = visit connections start []
      paths' = trace'' showPaths paths
   in length paths
  where
    isValidDirection (from, to) = from /= end && to /= start

showPaths :: [[(String, String)]] -> String
showPaths paths = unlines $ map showPath paths
  where
    showPath path = unwords (start : reverse (map snd path))

visit :: [(String, String)] -> String -> [(String, String)] -> [[(String, String)]]
visit [] _ path = []
visit remainingConnections currentCave path =
  if currentCave == end
    then [path]
    else
      let isLargeOrNotThisCave cave = (isUpper . head) cave || (cave /= currentCave)
          connectionsExceptReturnToThisSmall = filter (isLargeOrNotThisCave . snd) remainingConnections
          nextConnections = filter ((== currentCave) . fst) connectionsExceptReturnToThisSmall
          visitNext connection = visit (delete connection connectionsExceptReturnToThisSmall) (snd connection) (connection : path)
       in concatMap visitNext nextConnections

test1 =
  ( 10,
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
  ( 19,
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