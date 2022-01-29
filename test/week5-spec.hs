import Week5.Week5
import Week5.ExprT

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "exercise 1" $ do
    it "should return correct result of eval'" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
      eval (Add (Mul (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 10

  describe "exercise 2" $ do
    it "should return correct result of evalStr'" $ do
       evalStr "(2+3)*4" `shouldBe` Just 20
       evalStr "2+3*4" `shouldBe` Just 14
       evalStr "2+3*" `shouldBe` Nothing