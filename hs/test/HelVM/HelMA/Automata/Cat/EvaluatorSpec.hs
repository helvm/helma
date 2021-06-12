module HelVM.HelMA.Automata.Cat.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.Cat.Evaluator

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.MockIO

import Test.Hspec

spec :: Spec
spec = do

  describe "monadic" $ do
    it "Hello , world!" $ do (batchExecMockIO . eval) hw `shouldBe` toText hw

hw :: Source
hw = "#!/bin/cat\nHello , world!\n"