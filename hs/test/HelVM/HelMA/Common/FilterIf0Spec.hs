module HelVM.HelMA.Common.FilterIf0Spec (spec) where

import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.IO.MockIO

import Test.Hspec

wFilterIf0 :: WrapperIO m => m ()
wFilterIf0 = do
  char <- wGetChar
  if char == '0'
    then do
      wLogStrLn ""
      wPutChar '\n'
    else do
      wPutChar char
      wFilterIf0

spec :: Spec
spec = do
  describe "Test WFilter0" $ do
    it "Test WFilterIf0 with execMockIO" $ do execMockIO wFilterIf0 "qwerty0uiop" `shouldBe` "qwerty\n"
    it "Test WFilterIf0 with execMockIO" $ do evalMockIO wFilterIf0 "qwerty0uiop" `shouldBe` "\n"
