import Week2.LogAnalysis
import Week2.Log

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
  let message1 = LogMessage Info 1000 "Message"
  let message2 = LogMessage Info 2000 "Message"
  let message3 = LogMessage Info 3000 "Message"
  let message4 = LogMessage Info 1500 "Message"
  let message5 = LogMessage Info 2500 "Message"
  
  let leafTree1 = Node Leaf message1 Leaf
  let leafTree2 = Node Leaf message2 Leaf
  let leafTree3 = Node Leaf message3 Leaf
  let leafTree4 = Node Leaf message4 Leaf
  let leafTree5 = Node Leaf message5 Leaf

  let tree1 = Node leafTree1 message2 leafTree3
  let tree2 = Node (Node Leaf message1 leafTree4) message2 leafTree3
  let tree3 = Node leafTree1 message2 (Node leafTree5 message3 Leaf)
 
  describe "Test parseMessage" $ do
    it "should return correct message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

  describe "Test parse" $ do
    it "should return correct messages" $ do
      testParse parse 3 "app/Week2/error.log" `shouldReturn`
        [
          LogMessage Info 5053 "pci_id: con ing!",
          LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",
          LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
        ]

  describe "Test insert" $ do
    it "should return unchange input if LogMessage has no timestamp" $ do
      insert (Unknown "Message without timestamp") leafTree1 `shouldBe` leafTree1
    it "should return new Node if tree is Leaf" $ do
      insert message1 Leaf `shouldBe` leafTree1
    it "should return correct Tree if timestamp is less than first message" $ do
      insert message4 tree1 `shouldBe` tree2
    it "should return correct Tree if timestamp is greater than first message" $ do
      insert message4 tree1 `shouldBe` tree2

  describe "Test build" $ do
    it "should return Leaf is log list is empty" $ do
      build [] `shouldBe` Leaf
    it "should build Tree correctly" $ do
      let logs1 = [message1, message2, message3, message4, message5]          
      let result1 = Node Leaf message1 (Node leafTree4 message2 (Node leafTree5 message3 Leaf))
      build logs1 `shouldBe` result1
      
      let logs2 = [message3, message4, message2, message5, message1]          
      let result2 = Node (Node leafTree1 message4 (Node Leaf message2 leafTree5)) message3 Leaf
      build logs2 `shouldBe` result2
