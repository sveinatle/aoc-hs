module Main where

import Control.Monad (zipWithM_)
import Control.Monad.Loops (takeWhileM)
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
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
  let days =
        [ Day01.cases,
          Day02.cases,
          Day03.cases,
          Day04.cases,
          Day05.cases,
          Day06.cases,
          Day07.cases,
          Day08.cases,
          Day09.cases,
          Day10.cases,
          Day11.cases,
          Day12.cases,
          Day13.cases,
          Day14.cases,
          Day15.cases
        ]
  args <- getArgs
  case args of
    [dayStr] -> do
      let dayNum = read dayStr
          cases = days !! (dayNum -1)
      runDayCases dayNum cases
      return ()
    _ -> do
      let dayCount = length days
      putStrLn $ "No day specified. Running all " ++ show dayCount ++ " days..."
      zipWithM_ runDayCases [1 ..] days

getFilename dayNum caseName = "data/Day" ++ padDay dayNum ++ caseName ++ ".txt"
  where
    padDay dayNum = printf "%02d" dayNum

runDayCases :: Int -> [Case] -> IO ()
runDayCases dayNum cases = do
  takeWhileM (solveCase dayNum) cases
  return ()

solveCase dayNum (Case solver caseName expectedResult) = do
  caseData <- loadData $ getFilename dayNum caseName
  let caseResult = solver caseData
      testPassed
        | expectedResult == 0 = do
          putStrLn $ green $ "Day " ++ show dayNum ++ " problem result = " ++ show caseResult
          return True
        | caseResult == expectedResult = do
          putStrLn $ green $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] succeeded. Result = " ++ show caseResult
          return True
        | otherwise = do
          putStrLn $ red $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] failed. Result = " ++ show caseResult
          return False
  testPassed
