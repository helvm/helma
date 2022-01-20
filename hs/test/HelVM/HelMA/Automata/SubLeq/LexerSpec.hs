module HelVM.HelMA.Automata.SubLeq.LexerSpec (spec) where

import           HelVM.HelMA.Automata.SubLeq.FileUtil
import           HelVM.HelMA.Automata.SubLeq.Lexer

import           HelVM.Expectations

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec =
  describe "minification" $ forM_
    [ ("hello"     , "hello")
    , ("longHello" , "hello")
    ] $ \(input , output) ->
    it input $ show . readSymbols <$> readSqFile input `ioShouldBe` readSqFile output
