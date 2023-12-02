module DayProblem where

data Case
  = Case {solverInt :: [String] -> Int, caseName :: String, expectedResult :: Int}
  | CaseStr {solverStr :: [String] -> String, caseName :: String, expectedResultStr :: String}
  | Problem {solver :: [String] -> Int, caseName :: String}
  | ProblemStr {solverStr :: [String] -> String, caseName :: String}
