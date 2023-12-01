{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative
import qualified Day01.Trebuchet as Day01
import qualified Options.Applicative as Opt

newtype Options = Options {getDay :: Maybe Int} deriving (Show)

options :: Opt.Parser Options
options = Options <$> Applicative.optional (Opt.option Opt.auto $ Opt.long "day" <> Opt.help "run solution of a single day" <> Opt.metavar "DAY")

solutions :: [(Puzzle.Solver, String)]
solutions = [(Day01.solver, "data/day-01.txt")]

year :: String
year = "2022"

title :: String
title = "🎅🎄 Advent of Code " ++ year ++ " 🎄🦌"

main :: IO ()
main = do
  opts <-
    Opt.execParser
      ( Opt.info
          (options <**> Opt.helper)
          (Opt.fullDesc <> Opt.progDesc ("run advent of code " ++ year ++ " solutions") <> Opt.header title)
      )

  putStrLn title
  runSolutions (getDay opts)

runSolutions :: Maybe Int -> IO ()
runSolutions day = mapM_ runSolution . puzzles $ solutions
  where
    puzzles = case day of
      Nothing -> id
      Just day' -> filter ((== day') . Puzzle.day . fst)

runSolution :: (Puzzle.Solver, String) -> IO ()
runSolution (solver, file) = do
  putStrLn ""
  putStrLn $ "✨ Day " ++ show (Puzzle.day solver) ++ " : " ++ Puzzle.name solver ++ " ✨"
  putStrLn ""

  input <- readFile file
  let solution = Puzzle.solve solver input

  putStrLn $ "  ⭐ Part One : " ++ fst solution
  putStrLn $ "  ⭐ Part Two : " ++ snd solution
