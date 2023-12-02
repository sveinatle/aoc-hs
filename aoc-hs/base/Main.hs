module Main where

import Control.Monad (zipWithM_)
import Control.Monad.Loops (takeWhileM)
import DayProblem
import Debug.Trace
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

loadAndRunCase dayNum solver caseName = do
  caseData <- loadData $ getFilename dayNum caseName
  return $ solver caseData

checkTestResult dayNum caseName caseResult expectedResult shower =
  if caseResult == expectedResult
    then do
      putStrLn $ green $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] succeeded. Result = " ++ shower caseResult
      return True
    else do
      putStrLn $ red $ "Day " ++ show dayNum ++ " test case [" ++ caseName ++ "] failed. Result = " ++ shower caseResult ++ "\nExpected = " ++ shower expectedResult
      return False

stringResultHandler str = if elem '\n' str then '\n' : str else str -- Prefix newline to get left-aligned multi-line output.

solveCase dayNum (Case solver caseName expectedResult) = do
  caseResult <- loadAndRunCase dayNum solver caseName
  checkTestResult dayNum caseName caseResult expectedResult show
solveCase dayNum (CaseStr solver caseName expectedResult) = do
  caseResult <- loadAndRunCase dayNum solver caseName
  checkTestResult dayNum caseName caseResult expectedResult stringResultHandler
solveCase dayNum (Problem solver caseName) = do
  caseResult <- loadAndRunCase dayNum solver caseName
  putStrLn $ green $ "Day " ++ show dayNum ++ " problem result = " ++ show caseResult
  return True
solveCase dayNum (ProblemStr solver caseName) = do
  caseResult <- loadAndRunCase dayNum solver caseName
  putStrLn $ green $ "Day " ++ show dayNum ++ " problem result = " ++ stringResultHandler caseResult
  return True
