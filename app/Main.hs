module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import DayProblem
import System.Environment (getArgs)
import Text.Printf (printf)

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
  let days = [Day01.problems, Day02.problems, Day03.problems, Day04.problems, Day05.problems, Day06.problems, Day07.problems]
  args <- getArgs
  case args of
    [dayStr] -> do
      let dayNum = read dayStr
          day = days !! (dayNum -1)
          padDay dayNum = printf "%02d" dayNum
          filename dayNum typ = "data/Day" ++ padDay dayNum ++ typ ++ ".txt"

      testData <- loadData $ filename dayNum "Test"
      problemData <- loadData $ filename dayNum "Problem"

      solveProblem (fst day) testData problemData
      solveProblem (snd day) testData problemData
    _ -> putStrLn "Specify day number."

solveProblem (P solver expectedTestResult) testData problemData =
  let testResult = solver testData
   in if testResult == expectedTestResult
        then putStrLn $ green $ "Test SUCCEEDED. Problem result = " ++ show (solver problemData)
        else putStrLn $ red $ "Test FAILED. Result: " ++ show testResult
