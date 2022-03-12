module Main where

import Data.List (tails)
import Data.String (IsString)
import qualified MyLib (someFunc)

loadData :: String -> IO [String]
loadData filename = do
  filedata <- readFile filename
  return (lines filedata)

main :: IO ()
main = do
  testData <- loadData "app/Day01Test.txt"
  problemData <- loadData "app/Day01Problem.txt"

  let testResult = solveA2 testData
   in if testResult == 7
        then putStrLn $ "Test A SUCCEEDED. Problem result = " ++ show (solveA2 problemData)
        else putStrLn $ "Test A FAILED. Result: " ++ show testResult

  let testResult = solveB testData
   in if testResult == 5
        then putStrLn $ "Test B SUCCEEDED. Problem result = " ++ show (solveB problemData)
        else putStrLn $ "Test B FAILED. Result: " ++ show testResult

solveA :: [String] -> Int
solveA lines = countInc 0 (map read lines)

countInc :: Int -> [Int] -> Int
countInc sum [] = sum
countInc sum [a] = sum
countInc sum (a : b : rest)
  | a < b = countInc (sum + 1) (b : rest)
  | otherwise = countInc sum (b : rest)

solveA2 :: [String] -> Int
solveA2 lines = countIfInc $map read lines

countIfInc :: [Int] -> Int
countIfInc nums =
  let pairs = zip nums (tail nums)
   in length $ filter (uncurry (<)) pairs

solveB :: [String] -> Int
solveB lines = countIfInc $sumBy3 $map read lines

sumBy3 :: [Int] -> [Int]
sumBy3 nums =
  let threes = zip3 nums (tail nums) (tail $tail nums)
      sum3 (x, y, z) = x + y + z
   in map sum3 threes