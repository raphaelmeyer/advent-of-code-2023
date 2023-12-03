{-# LANGUAGE OverloadedStrings #-}

module Day02.CubeConundrumSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day02.CubeConundrum as Cube
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

spec :: Spec
spec = do
  describe "Cube Conundrum" $ do
    let input = Cube.parseInput exampleInput

    it "should sum possible game ids" $ do
      Cube.partOne input `shouldBe` "8"

    it "should parse a game" $ do
      length input `shouldBe` 5
      let game = input !! 2

      Cube.getId game `shouldBe` 3
      length (Cube.getReveals game) `shouldBe` 3

      let reveal = Cube.getReveals game !! 2
      Map.lookup Cube.Red reveal `shouldBe` Just 1
      Map.lookup Cube.Green reveal `shouldBe` Just 5
      Map.lookup Cube.Blue reveal `shouldBe` Nothing

    it "should check if a game is possible" $ do
      let possible = input !! 1
      possible `shouldSatisfy` Cube.possibleGame

      let impossible = input !! 2
      impossible `shouldNotSatisfy` Cube.possibleGame

    it "should sum power of minimum sets" $ do
      Cube.partTwo input `shouldBe` "2286"

    it "should calculate the power of minimum sets" $ do
      Cube.minimumPower (input !! 2) `shouldBe` 1560
      Cube.minimumPower (input !! 4) `shouldBe` 36
