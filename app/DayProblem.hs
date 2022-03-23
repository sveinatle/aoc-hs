module DayProblem where

data Case
  = Case {solver :: [String] -> Int, caseName :: String, expectedResult :: Int}
  | Problem {solver :: [String] -> Int, caseName :: String}
