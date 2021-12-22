import Week3.Golf

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "localMaxima" $ do
    it "should return [] if input is []" $ do
      localMaxima [] `shouldBe` []

    it "should return correct list of local maximas" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []
      
    it "should work for first and last elem of list as highest number" $ do
      localMaxima [9,3,4,1,5] `shouldBe` [4]
      localMaxima [9,3,4,7,5] `shouldBe` [7]
      localMaxima [9,3,4,2,1] `shouldBe` [4]

  describe "Test skips" $ do
    it "should return correct list" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True,False] `shouldBe` [[True,False], [False]]
      skips "" `shouldBe` []

    it "should return every nth elem in a list" $ do
      takeEveryNth 99 "" `shouldBe` []
      takeEveryNth 0 "ABCD" `shouldBe` []
      takeEveryNth 1 "ABCD" `shouldBe` "ABCD"
      takeEveryNth 2 "ABCD" `shouldBe` "BD"
      takeEveryNth 3 "ABCD" `shouldBe` "C"
      takeEveryNth 3 "ABCDEFGH" `shouldBe` "CF"