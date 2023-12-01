module Day01.Trebuchet where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Tuple.Extra as Tuple

solver :: Puzzle.Solver
solver = Puzzle.Solver 1 "📜 Trebuchet" solve

solve :: String -> Puzzle.Solution
solve = Tuple.both show . (partTwo &&& partOne) . Text.lines . Text.pack

-- solution

partOne :: [Text.Text] -> Int
partOne _ = 0

partTwo :: [Text.Text] -> Int
partTwo _ = 0
