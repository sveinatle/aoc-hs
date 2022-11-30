module Main where

import Control.Monad (zipWithM_)
import Control.Monad.Loops (takeWhileM)
import DayProblem
import System.Environment (getArgs)
import Text.Printf (printf)
import Year (dataPath, days)

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

getFilename dayNum caseName = dataPath ++ "Day" ++ padDay dayNum ++ caseName ++ ".txt"
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
        | caseResult == expectedResult = do
          putStrLn $ green $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] succeeded. Result = " ++ show caseResult
          return True
        | otherwise = do
          putStrLn $ red $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] failed. Result = " ++ show caseResult
          return False
  testPassed
solveCase dayNum (Problem solver caseName) = do
  caseData <- loadData $ getFilename dayNum caseName
  let caseResult = solver caseData
   in putStrLn $ green $ "Day " ++ show dayNum ++ " problem result = " ++ show caseResult
  return True
