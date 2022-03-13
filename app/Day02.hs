module Main where

loadData :: String -> IO [String]
loadData filename = do
  filedata <- readFile filename
  return (lines filedata)

main :: IO ()
main = do
  testData <- loadData "app/Day02Test.txt"
  problemData <- loadData "app/Day02Problem.txt"

  let testResult = solveA testData
   in if testResult == 150
        then putStrLn $ "Test A SUCCEEDED. Problem result = " ++ show (solveA problemData)
        else putStrLn $ "Test A FAILED. Result: " ++ show testResult

  let testResult = solveB testData
   in if testResult == 900
        then putStrLn $ "Test B SUCCEEDED. Problem result = " ++ show (solveB problemData)
        else putStrLn $ "Test B FAILED. Result: " ++ show testResult

solveA :: [String] -> Int
solveA lines =
  let (x, y) = travel1 $map text2pair lines
   in x * y

solveB :: [String] -> Int
solveB lines =
  let (x, y) = travel2 $map text2pair lines
   in x * y

text2pair :: String -> (Int, Int)
text2pair line =
  let [dir, mag] = words line
      matcher dir mag
        | dir == "up" = (0, - mag)
        | dir == "down" = (0, mag)
        | dir == "forward" = (mag, 0)
        | otherwise = error "unexpected dir"
   in matcher dir (read mag)

travel1 :: [(Int, Int)] -> (Int, Int)
travel1 deltas =
  let traveller (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
   in foldl traveller (0, 0) deltas

travel2 :: [(Int, Int)] -> (Int, Int)
travel2 deltas =
  let accumulateAim (_, accumulatedAim) (x, deltaAim) = (x, accumulatedAim + deltaAim)
      accumulatedAim = scanl accumulateAim (0, 0) deltas
      traveller (accX, accDepth) (x, aim) = (accX + x, accDepth + x * aim)
   in foldl traveller (0, 0) accumulatedAim