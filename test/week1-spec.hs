import Week1

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "Tests toDigits" $ do
    it "should return correct list" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []

  describe "Tests toDigitsRev" $ do
    it "should return correct list" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

  describe "Tests doubleEveryOther" $ do
    it "should return correct list" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
      doubleEveryOther [] `shouldBe` []
      doubleEveryOther [1] `shouldBe` [1]

  describe "Tests sumDigits" $ do
    it "should return correct number" $ do
      sumDigits [16,7,12,5] `shouldBe` 1 + 6 + 7 + 1 + 2 + 5
      sumDigits [] `shouldBe` 0

  describe "Tests validate" $ do
    it "should valitdate credit card number" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False
