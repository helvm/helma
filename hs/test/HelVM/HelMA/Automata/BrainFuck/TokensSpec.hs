module HelVM.HelMA.Automata.BrainFuck.TokensSpec (spec) where

import           HelVM.HelMA.Automata.BrainFuck.FileUtil
import           HelVM.HelMA.Automata.BrainFuck.Lexer

import           HelVM.HelMA.Automaton.WrapTokenList

import           HelVM.Expectations

import           Test.Hspec                              (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "tokenize" $ do
    it "helloWorld"             $ show . readTokens                   <$> readBfFile "helloWorld"             `ioShouldBe`   readBfFile "helloWorld"
    it "helloWorldWithComments" $ show . readTokens                   <$> readBfFile "helloWorldWithComments" `ioShouldBe`   readBfFile "helloWorld"
    it "helloWorldAsList"       $ show . unWrapTokenList . readTokens <$> readBfFile "helloWorldWithComments" `shouldReturn` helloWorldAsList

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"
