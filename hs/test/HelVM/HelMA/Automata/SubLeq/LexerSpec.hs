module HelVM.HelMA.Automata.SubLeq.LexerSpec (spec) where

import           HelVM.HelMA.Automata.SubLeq.FileUtil
import           HelVM.HelMA.Automata.SubLeq.Lexer

import           HelVM.Expectations

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec =
  describe "minification" $ do
    it "hello"     $ show . readSymbols <$> readSqFile "hello"     `ioShouldBe` readSqFile "hello"
    it "longHello" $ show . readSymbols <$> readSqFile "longHello" `ioShouldBe` readSqFile "hello"
