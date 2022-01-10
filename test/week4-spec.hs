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
