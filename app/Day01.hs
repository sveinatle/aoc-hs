module Main where

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

  let testResult = solve testData
  if testResult == 7
    then putStrLn $ "Test SUCCEEDED. Problem result = " ++ show (solve problemData)
    else putStrLn $ "Test FAILED. Result: " ++ show testResult

solve :: [String] -> Int
solve vs = countInc 0 (map read vs)

countInc :: Int -> [Int] -> Int
countInc sum [] = sum
countInc sum [a] = sum
countInc sum (a : b : rest)
  | a < b = countInc (sum + 1) (b : rest)
  | otherwise = countInc sum (b : rest)

-- Use foldl or something?