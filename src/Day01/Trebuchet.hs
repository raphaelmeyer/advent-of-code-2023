{-# LANGUAGE OverloadedStrings #-}

module Day01.Trebuchet where

import qualified AoC.Puzzle as Puzzle
import Data.Char (isDigit)
import qualified Data.Either as Either
import qualified Data.List as List
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
partTwo = show . sum . map recoverRealValue

recoverValue :: Text.Text -> Int
recoverValue line = fst . Either.fromRight (0, Text.empty) . Read.decimal . Text.pack $ number
  where
    onlyDigits = Text.filter isDigit line
    number = [Text.head onlyDigits, Text.last onlyDigits]

recoverRealValue :: Text.Text -> Int
recoverRealValue line = findFirstOf line * 10 + findLastOf line

type Digit = (Text.Text, Int)

digits :: [Digit]
digits =
  [ ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

findFirstOf :: Text.Text -> Int
findFirstOf "" = error "no solution"
findFirstOf calibration = case List.find ((`Text.isPrefixOf` calibration) . fst) digits of
  Just (_, value) -> value
  Nothing -> findFirstOf (Text.tail calibration)

findLastOf :: Text.Text -> Int
findLastOf "" = error "no solution"
findLastOf calibration = case List.find ((`Text.isSuffixOf` calibration) . fst) digits of
  Just (_, value) -> value
  Nothing -> findLastOf (Text.init calibration)
