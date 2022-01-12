module HelVM.HelMA.Automaton.FilterIf0Spec (spec) where

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.Common.Control.Control

import           Test.Hspec                          (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

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
spec =
  describe "Test WFilter0" $ do
    let mockIO = runMockIO "qwerty0uiop" $ safeWithMessages <$> wFilterIf0
    forM_ [ ("Test WFilterIf0 with calculateOutput" , calculateOutput , "qwerty\n")
          , ("Test WFilterIf0 with calculateLogged" , calculateLogged , "\n"      )
          ] $ \(name , action , output) ->
      it name $ action mockIO `shouldBe` output
