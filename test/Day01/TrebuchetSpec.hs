{-# LANGUAGE OverloadedStrings #-}

module Day01.TrebuchetSpec where

import qualified Data.Text as Text
import Day01.Trebuchet as Trebuchet
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  ]

exampleTwo :: [Text.Text]
exampleTwo =
  [ "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  ]

spec :: Spec
spec = do
  describe "Trebuchet" $ do
    it "should calculate the sum of calibration values" $ do
      Trebuchet.partOne exampleInput `shouldBe` "142"

    it "should recover calibration values" $ do
      map Trebuchet.recoverValue exampleInput `shouldBe` [12, 38, 15, 77]

    it "should calculate the sum of the real calibration values" $ do
      Trebuchet.partTwo exampleTwo `shouldBe` "281"

    it "should recover the real calibration values" $ do
      map Trebuchet.recoverRealValue exampleTwo `shouldBe` [29, 83, 13, 24, 42, 14, 76]
