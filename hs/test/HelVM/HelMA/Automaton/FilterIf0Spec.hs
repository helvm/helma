module HelVM.HelMA.Automaton.FilterIf0Spec (spec) where

import HelVM.HelMA.Automaton.IO.BusinessIO
import HelVM.HelMA.Automaton.IO.MockIO

import Test.Hspec (Spec , describe , it)
import Test.Hspec.Expectations.Pretty

wFilterIf0 :: BusinessIO m => m ()
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
    let mockIO = execMockIO wFilterIf0 "qwerty0uiop"
    it "Test WFilterIf0 with calculateOutput" $ do calculateOutput mockIO `shouldBe` "qwerty\n"
    it "Test WFilterIf0 with calculateLogged" $ do calculateLogged mockIO `shouldBe` "\n"
