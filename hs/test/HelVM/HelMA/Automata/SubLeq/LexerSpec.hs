module HelVM.HelMA.Automata.SubLeq.LexerSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Lexer
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.HelMA.Automata.Expectations

import Test.Hspec

spec :: Spec
spec = do
  describe "minification" $ do
    it "hello"     $ do show . readSymbols <$> readSqFile "hello"     `ioShouldBe` readSqFile "hello"
    it "longHello" $ do show . readSymbols <$> readSqFile "longHello" `ioShouldBe` readSqFile "hello"
