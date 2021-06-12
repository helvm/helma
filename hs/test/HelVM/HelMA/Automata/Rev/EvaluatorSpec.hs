module HelVM.HelMA.Automata.Rev.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.Rev.Evaluator

import HelVM.HelMA.Automaton.IO.MockIO
import HelVM.HelMA.Automaton.API.IOTypes

import Test.Hspec

spec :: Spec
spec = do
  describe "monadic" $ do
    it "Hello, world!" $ do (batchExecMockIO . eval) hw `shouldBe` hwo

hw :: Source
hw = "#!/usr/bin/rev\n!dlrow ,olleH\n"

hwo :: Output
hwo = "ver/nib/rsu/!#\nHello, world!\n"