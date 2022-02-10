import Week6.Week6

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
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