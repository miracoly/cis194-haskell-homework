import Week2.LogAnalysis
import Week2.Log

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Exit (exitFailure)

main = hspec $ do
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
