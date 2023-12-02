module Day07 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import DayProblem
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

trace'' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 95437,
    Problem solveA "Problem",
    Case solveB "Test" 24933642,
    Problem solveB "Problem"
  ]

data FilesystemEntryContent
  = Directory [FilesystemEntry]
  | File Int
  deriving (Show)

type FilesystemEntry = (String, FilesystemEntryContent)

type Crumb = FilesystemEntry

type Crumbs = [Crumb]

type Zipper = (FilesystemEntry, Crumbs)

type State = (Zipper, [String])

readDirectory :: String -> [String] -> FilesystemEntry
readDirectory name lines = (name, Directory (map readDirectoryEntry lines))

readDirectoryEntry :: [Char] -> FilesystemEntry
readDirectoryEntry line = case splitOn " " line of
  ["dir", name] -> (name, Directory [])
  [sizeStr, name] -> (name, File (read sizeStr))
  _ -> error $ "Unexpected entry line: " ++ line

replaceEntry :: [FilesystemEntry] -> FilesystemEntry -> [FilesystemEntry]
replaceEntry entries (newEntryName, newEntryValue) =
  let (before, _ : after) = break ((== newEntryName) . fst) entries
   in before ++ (newEntryName, newEntryValue) : after

takeEntry :: [FilesystemEntry] -> String -> FilesystemEntry
takeEntry entries name = case find ((== name) . fst) entries of
  Just entry -> entry
  Nothing -> error $ "Child not found: " ++ name

cd :: String -> Zipper -> Zipper
cd name zipper = case name of
  ".." ->
    let (currentDirectory, (parentName, Directory parentEntries) : crumbs) = zipper
        newParent = (parentName, Directory (replaceEntry parentEntries currentDirectory))
     in (newParent, crumbs)
  "/" -> until (null . snd) (cd "..") zipper
  name ->
    let (currentDirectory, crumbs) = zipper
        (_, Directory children) = currentDirectory
     in (takeEntry children name, currentDirectory : crumbs)

readProcess :: State -> State
readProcess (zipper, line : lines) =
  let ((currentDirectoryName, Directory currentEntries), crumbs) = zipper
   in case splitOn " " line of
        ["$", "ls"] ->
          let (directoryLines, restLines) = break ((== '$') . head) lines
              directory = readDirectory currentDirectoryName directoryLines
           in if (not . null) currentEntries
                then (zipper, restLines) -- Don't replace entries if we have already loaded them (would lose info about contents in sub-folders).
                else ((directory, crumbs), restLines)
        ["$", "cd", name] -> (cd name zipper, lines)
        _ -> error $ "Unexpected command: " ++ line
readProcess (_, []) = error "No more lines"

collectDirectorySizes :: FilesystemEntry -> (Int, [(String, Int)])
collectDirectorySizes (name, Directory children) =
  let sizes = map collectDirectorySizes children
      totalSize = sum $map fst sizes
   in (totalSize, (name, totalSize) : concatMap snd sizes)
collectDirectorySizes (name, File size) = (size, [])

readSizes :: [String] -> [(String, Int)]
readSizes lines =
  let initialState = ((("/", Directory []), []), lines)
      stateAfterRead = cd "/" . fst $ until (null . snd) readProcess initialState
   in snd $ collectDirectorySizes $ fst stateAfterRead

solveA :: [String] -> Int
solveA = sum . filter (<= 100000) . map snd . readSizes

solveB :: [String] -> Int
solveB lines =
  let sizes = map snd $readSizes lines
      usedSize = maximum sizes
      deleteSize = 30000000 - (70000000 - usedSize)
   in minimum $ filter (>= deleteSize) sizes