{-# LANGUAGE OverloadedStrings #-}

module Day02.CubeConundrum where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 2 "👜 Cube Conundrum" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [Game] -> String
partOne = show . sum . map getId . filter possibleGame

partTwo :: [Game] -> String
partTwo _ = ""

data Cube = Red | Green | Blue deriving (Eq, Ord, Show)

type Hand = Map.Map Cube Int

data Game = Game {getId :: Int, getReveals :: [Hand]} deriving (Eq, Show)

constraints :: Hand
constraints = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

possibleGame :: Game -> Bool
possibleGame = all possibleHand . getReveals

possibleHand :: Hand -> Bool
possibleHand hand = all (possible hand) [Red, Green, Blue]

possible :: Hand -> Cube -> Bool
possible hand cube = case (Map.lookup cube constraints, Map.lookup cube hand) of
  (Just c, Just h) -> h <= c
  _ -> True

-- parse Input

parseInput :: [Text.Text] -> [Game]
parseInput = map parseGame

type Parser = MP.Parsec Void Text.Text

parseGame :: Text.Text -> Game
parseGame input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right r -> r
  where
    pId :: Parser Int
    pId = token "Game" *> number <* token ":"
    grammar :: Parser Game
    grammar = Game <$> pId <*> parseHands

parseHands :: Parser [Hand]
parseHands = MP.sepBy parseHand (token ";")

parseHand :: Parser Hand
parseHand = Map.fromList <$> MP.sepBy parseCubes (token ",")

parseCubes :: Parser (Cube, Int)
parseCubes = flip (,) <$> number <*> (pRed <|> pGreen <|> pBlue)
  where
    pRed = Red <$ token "red"
    pGreen = Green <$ token "green"
    pBlue = Blue <$ token "blue"

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s) :: Parser Text.Text

number :: Parser Int
number = L.lexeme C.space L.decimal
