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
    it "lit, mul & add should work for instance MinMax" $ do
      minMaxify (lit 2) `shouldBe` MinMax 2
      minMaxify (lit (-3)) `shouldBe` MinMax (-3)
      minMaxify (mul (lit (-3)) (lit 2)) `shouldBe` MinMax (-3)
      minMaxify (mul (lit 3) (lit 2)) `shouldBe` MinMax 2
      minMaxify (add (lit (-3)) (lit 2)) `shouldBe` MinMax 2
      minMaxify (add (lit 3) (lit 2)) `shouldBe` MinMax 3
      minMaxify (add (lit (-3)) (lit (-2))) `shouldBe` MinMax (-2)
    it "lit, mul & add should work for instance Mod7" $ do
      mod7ify (lit 2) `shouldBe` Mod7 2
      mod7ify (lit 7) `shouldBe` Mod7 0
      mod7ify (lit 8) `shouldBe` Mod7 1
      mod7ify (lit (-3)) `shouldBe` Mod7 4
      mod7ify (lit (-7)) `shouldBe` Mod7 0
      mod7ify (mul (lit (-3)) (lit 2)) `shouldBe` Mod7 1
      mod7ify (mul (lit 3) (lit 2)) `shouldBe` Mod7 6
      mod7ify (add (lit (-3)) (lit 2)) `shouldBe` Mod7 6
      mod7ify (add (lit 3) (lit 2)) `shouldBe` Mod7 5
      mod7ify (add (lit (-3)) (lit (-2))) `shouldBe` Mod7 2
