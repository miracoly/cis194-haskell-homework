import Week5.Week5
import Week5.ExprT

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "exercise 1" $ do
    it "should return correct result of eval" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
      eval (Add (Mul (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 10

  describe "exercise 2" $ do
    it "should return correct result of evalStr" $ do
       evalStr "(2+3)*4" `shouldBe` Just 20
       evalStr "2+3*4" `shouldBe` Just 14
       evalStr "2+3*" `shouldBe` Nothing

  describe "exercise 4" $ do
    it "lit, mul & add should work for instance Integer" $ do
      integify (lit 2) `shouldBe` 2
      integify (add 2 3) `shouldBe` 5
      integify (mul 2 3) `shouldBe` 6
    it "lit, mul & add should work for instance Bool" $ do
      boolify (lit 2) `shouldBe` True
      boolify (lit (-3)) `shouldBe` False
      boolify (mul (lit (-3)) (lit 2)) `shouldBe` False
      boolify (mul (lit 3) (lit 2)) `shouldBe` True
      boolify (add (lit (-3)) (lit 2)) `shouldBe` True
      boolify (add (lit 3) (lit 2)) `shouldBe` True
      boolify (add (lit (-3)) (lit (-2))) `shouldBe` False