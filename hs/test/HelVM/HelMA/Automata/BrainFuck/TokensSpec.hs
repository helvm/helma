module HelVM.HelMA.Automata.BrainFuck.TokensSpec (spec) where

import           HelVM.HelMA.Automata.BrainFuck.FileUtil
import           HelVM.HelMA.Automata.BrainFuck.Lexer

import           HelVM.HelMA.Automaton.WrapTokenList

import           HelVM.Expectations

import           Test.Hspec                              (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "helloWorld"             $ do show . readTokens                   <$> readBfFile "helloWorld"             `ioShouldBe`   readBfFile "helloWorld"
    it "helloWorldWithComments" $ do show . readTokens                   <$> readBfFile "helloWorldWithComments" `ioShouldBe`   readBfFile "helloWorld"
    it "helloWorldAsList"       $ do show . unWrapTokenList . readTokens <$> readBfFile "helloWorldWithComments" `shouldReturn` helloWorldAsList

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"
