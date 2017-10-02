module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  let charset :: String
      charset = "abcdefghijklmnopqrstuvwxyz .,-"
      allowedStrRx :: String
      allowedStrRx = "[abcdefghijklmnopqrstuvwxyz .,-]"
  describe "ceasar" $ do
    it "case1" $ do
      (ceasarEncode "abcd" 1 charset allowedStrRx) `shouldBe` "bcde"
      (ceasarDecode "abcd" 1 charset allowedStrRx) `shouldBe` "-abc"
