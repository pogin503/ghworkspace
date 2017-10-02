module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  let charset :: String
      charset = "abcdefghijklmnopqrstuvwxyz .,-"
      allowedStrRx :: String
      allowedStrRx = "[abcdefghijklmnopqrstuvwxyz .,-]"
  describe "caesar" $ do
    it "case1" $ do
      (caesarEncode "abcd" 1 charset allowedStrRx) `shouldBe` "bcde"
      (caesarDecode "abcd" 1 charset allowedStrRx) `shouldBe` "-abc"
    it "answer" $ do
      let q = "qdq-gi.q-a ziatmxxitmdqibtqi-ustbi ri.qmoqrcxi.qbubu zir -ibtqi-qp-qaai ripmymsqkir -ibtqi-qy dmxi ri.cnxuoi rruoumxakir -ibtqiqzmobyqzbkii-q.qmxi -imyqzpyqzbi rixmeaki -puzmzoqai -i-qscxmbu zaimzpir -i btq-iymbbq-a;iz -iatmxximzgi.q-a zinqiuzimzgiemgipuao-uyuzmbqpimsmuzabir -ia. za -uzsiacotiimi.qbubu zj"
          answer = "every person shall have the right of peaceful petition for the redress of damage, for the removal of public officials, for the enactment,  repeal or amendment of laws, ordinances or regulations and for other matters; nor shall any person be in any way discriminated against for sponsoring such  a petition."
      (head $ solve q "person") `shouldBe` answer
