module HelVM.HelMA.Automaton.Eff.FreeEffSpec (spec) where

import           HelVM.HelMA.Automaton.Eff.FreeEff
import           HelVM.HelMA.Automaton.Eff.MockEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Control

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

wFilterIf0 :: MonadEff m => m ()
wFilterIf0 = do
  char <- getChar
  if char == '0'
    then do
      logInfo "\n"
      putChar '\n'
    else do
      putChar char
      wFilterIf0

spec :: Spec
spec = do
  describe "Test WFilter0" $ do
    let mockIO = runMockEff "qwerty0uiop" $ safeWithMessages <$> wFilterIf0
    forM_
      [ ("Test WFilterIf0 with calculateOutput" , calculateOutput , "qwerty\n")
      , ("Test WFilterIf0 with calculateLogged" , calculateLogged , "\n"      )
      ] $ \(name , action , output) ->
      it name $ action mockIO `shouldBe` output
  describe "Test Free WFilter0" $ do
    let mockIO = runMockEff "qwerty0uiop" $ safeWithMessages <$> interpretFreeEffToMonadEff (logOutputFree $ logInputFree wFilterIf0)
    forM_
      [ ("Test Free WFilterIf0 with calculateOutput" , calculateOutput , "qwerty\n"       )
      , ("Test Free WFilterIf0 with calculateLogged" , calculateLogged , "qqwweerrttyy0\n\n")
      ] $ \(name , action , output) ->
      it name $ action mockIO `shouldBe` output
