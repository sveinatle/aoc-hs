module Main where

import Data.List (transpose)

loadData :: String -> IO [String]
loadData filename = do
  filedata <- readFile filename
  return (lines filedata)

green :: String -> String
green str = "\27[32m" ++ str ++ "\27[0m"

red :: String -> String
red str = "\27[31m" ++ str ++ "\27[0m"

main :: IO ()
main = do
  testData <- loadData "app/Day03Test.txt"
  problemData <- loadData "app/Day03Problem.txt"

  let testResult = solveA testData
   in if testResult == 198
        then putStrLn $ green $ "Test A SUCCEEDED. Problem result = " ++ show (solveA problemData)
        else putStrLn $ red $ "Test A FAILED. Result: " ++ show testResult

  let testResult = solveB testData
   in if testResult == 230
        then putStrLn $ green $ "Test B SUCCEEDED. Problem result = " ++ show (solveB problemData)
        else putStrLn $ red $ "Test B FAILED. Result: " ++ show testResult

split2PlusMinus :: [Char] -> [Int]
split2PlusMinus = map (\c -> if c == '1' then 1 else -1)

bools2int :: [Bool] -> Int
bools2int bools =
  let boolsRev = reverse bools
      bools2int' [] = 0
      bools2int' (b : bs) = (if b then 1 else 0) + 2 * bools2int' bs
   in bools2int' boolsRev

solveA :: [String] -> Int
solveA lines =
  let plusMinusArrayPerBit = transpose $map split2PlusMinus lines
      bitSums = map sum plusMinusArrayPerBit
      gammaBools = map (> 0) bitSums
      gamma = bools2int gammaBools
      epsilon = bools2int $ map not gammaBools
   in gamma * epsilon

solveB :: [String] -> Int
solveB lines = 9999