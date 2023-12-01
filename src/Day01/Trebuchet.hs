module Day01.Trebuchet where

import qualified AoC.Puzzle as Puzzle
import Data.Char (isDigit)
import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 1 "📜 Trebuchet" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . Text.lines . Text.pack

-- solution

partOne :: [Text.Text] -> String
partOne = show . sum . map recoverValue

partTwo :: [Text.Text] -> String
partTwo _ = ""

recoverValue :: Text.Text -> Int
recoverValue line = fst . Either.fromRight (0, Text.empty) . Read.decimal . Text.pack $ number
  where
    digits = Text.filter isDigit line
    number = [Text.head digits, Text.last digits]
