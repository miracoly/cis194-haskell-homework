module Week6.Week6Spec (spec) where

import Week6.Week6

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

spec :: Spec
spec = do
  describe "exercise 1" $ do
    it "should return correct result of fib" $ do
      fib (-1) `shouldBe` 0
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 3 `shouldBe` 2
      fib 4 `shouldBe` 3
      fib 5 `shouldBe` 5
      fib 6 `shouldBe` 8
      fib 7 `shouldBe` 13
      fib 8 `shouldBe` 21
      fib 9 `shouldBe` 34
      fib 10 `shouldBe` 55

  describe "exercise 4" $ do
    it "should show correct output of Stream" $ do
      show (streamRepeat 1) `shouldBe` show (replicate 20 1)

  describe "exercise 4" $ do
    it "should return correct result of streamToList" $ do
      sum (take 20 (streamToList (streamRepeat 1))) `shouldBe` 20
    it "should return correct result of streamMap" $ do
      show (streamMap (+2) (streamRepeat 1)) `shouldBe` show (replicate 20 3)
    it "should return correct result of streamFromSeed" $ do
      show (streamFromSeed (+2) 1) `shouldBe` show [1,3..39]

  describe "exercise 5" $ do
    it "should return correct result of nats" $ do
      show nats `shouldBe` show [1..20]
    it "should return correct result of interleave" $ do
      show (interleave (streamRepeat 1) (streamRepeat 2)) `shouldBe` show (take 20 $ cycle [1,2])
    it "should return correct result of ruler" $ do
      show ruler `shouldBe` show [0,1,0,2,0,1,0,3,0,1,0,4,0,1,0,5,0,1,0,6]
