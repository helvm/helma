module HelVM.HelMA.Automaton.Eff.FreeEffSpec
    ( spec
    ) where

import           HelVM.HelMA.Automaton.Eff.FreeEff
import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

wFilterIf0 ∷ MonadEff m ⇒ m ()
wFilterIf0 = do
  char <- getChar
  if char == '0'
    then do
      putChar '\n'
    else do
      putChar char
      wFilterIf0

spec ∷ Spec
spec = do
  describe "Test WFilter0" $ do
    let mockEff = execMockEffWithInput "qwerty0uiop" wFilterIf0
    forM_
      [ ("Test WFilterIf0 with calculateOutput" , calculateOutput , "qwerty\n")
      , ("Test WFilterIf0 with calculateLogsWithLevelDebug" , calculateLogsWithLevelDebug , ""      )
      ] $ \(name , action , output) ->
      it name $ action mockEff `shouldBe` output
  describe "Test Free WFilter0" $ do
    let mockEff = execMockEffWithInput "qwerty0uiop" $ interpretFreeEffDebug wFilterIf0
    forM_
      [ ("Test Free WFilterIf0 with calculateOutput" , calculateOutput , "qwerty\n"       )
      , ("Test Free WFilterIf0 with calculateLogsWithLevelDebug" , calculateLogsWithLevelDebug , debugLogs)
      ] $ \(name , action , output) ->
      it name $ action mockEff `shouldBe` output

debugLogs ∷ Text
debugLogs =
  "Debug GetChar: q\n\
  \Debug PutChar: q\n\
  \Debug GetChar: w\n\
  \Debug PutChar: w\n\
  \Debug GetChar: e\n\
  \Debug PutChar: e\n\
  \Debug GetChar: r\n\
  \Debug PutChar: r\n\
  \Debug GetChar: t\n\
  \Debug PutChar: t\n\
  \Debug GetChar: y\n\
  \Debug PutChar: y\n\
  \Debug GetChar: 0\n\
  \Debug PutChar: \n\
  \\n"

