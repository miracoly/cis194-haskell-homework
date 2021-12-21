import Week3.Golf

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
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