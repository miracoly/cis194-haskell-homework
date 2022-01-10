import Week4.Week4

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "exercise 1" $ do
    it "should return correct result of fun1'" $ do
      fun1' [1,2,3,4,5,6,7,8] `shouldBe` 0
      fun1' [1,3,4,5,6,7,8] `shouldBe` 48

    it "should return correct result of fun2'" $ do
      fun2' 2 `shouldBe` 2
      fun2' 9 `shouldBe` 276
      fun2' 19 `shouldBe` 424
  
  -- describe "exercise 2" $ do
  --   it "should return correct result of foldTree'" $ do
  --     foldTree "ABCDEFGHIJ" `shouldBe`
  --       Node 3
  --         (Node 2
  --           (Node 0 Leaf 'F' Leaf)
  --           'I'
  --           (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
  --         'J'
  --         (Node 2
  --           (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
  --           'H'
  --           (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
  describe "exercise 3" $ do
    it "should return correct result of xor'" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

    it "should return correct result of map'" $ do
      map' (+1) [1,2,3,4,5] `shouldBe` [2,3,4,5,6]
      map' (++ "a") ["a","b","c","d","e"] `shouldBe` ["aa","ba","ca","da","ea"]
  
  describe "exercise 4" $ do
    it "should return correct result of sieveSundaram" $ do
      cartProd [1,2] ['a','b'] `shouldBe` [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
      sieveSundaram 7 `shouldBe` [3,5,7]
      sieveSundaram 12 `shouldBe` [3,5,7,11]
      sieveSundaram 24 `shouldBe` [3,5,7,11,13,17,19,23]