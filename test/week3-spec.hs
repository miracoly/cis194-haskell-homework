import Week3.Golf

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  describe "extractLines" $ do
    -- it "should drop first elem of every list elemt in list1" $ do
    --   dropFirst [[1,5], [1], [1]] `shouldBe` [[5]]

    it "should extract lines from list of Int lists" $ do
      extractLines [[1,1,1],[5]] `shouldBe` [[1,5], [1], [1]]
      extractLines [[1],[2],[3],[4,4,4,4],[5],[6,6],[9]]
        `shouldBe` [[1,2,3,4,5,6,9],[4,6],[4],[4]]
      -- extractLines [[3],[5]] `shouldBe` [[3,5]]

  describe "groupNumbers" $ do
    it "should transform list correctly" $ do
      groupNumbers [1,2,3,4,4,4,4,5,6,6,9]
        `shouldBe` [[1],[2],[3],[4,4,4,4],[5],[6,6],[9]]
      groupNumbers [1,1,1,5] `shouldBe` [[1,1,1], [5]]
      groupNumbers [3,5] `shouldBe` [[3],[5]]
  
  -- describe "drawLine" $ do
  --   it "should draw line of histogram out of list" $ do
  --     drawLine [0,1,2,3,4,5,6,7,8,9] `shouldBe` "**********"
  --     drawLine [0,2,4,6,8] `shouldBe` "* * * * * "
  --     drawLine [2,6] `shouldBe` "  *   *   "

  -- describe "histogram" $ do
  --   it "should return correct histogram string" $ do
  --     -- histogram [1,1,1,5] `shouldBe`
  --     --   " *        \n" ++
  --     --   " *        \n" ++ 
  --     --   " *   *    \n" ++
  --     --   "==========\n" ++
  --     --   "0123456789\n"
  --     -- histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe`
  --     --   "    *     \n" ++
  --     --   "    *     \n" ++
  --     --   "    * *   \n" ++
  --     --   " ******  *\n" ++
  --     --   "==========\n" ++
  --     --   "0123456789\n"
  --     -- histogram [3,5] `shouldBe`
  --     --   "   * *    \n" ++
  --     --   "==========\n" ++
  --     --   "0123456789\n"
  --     histogram [] `shouldBe`
  --       "==========\n" ++
  --       "0123456789\n"

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