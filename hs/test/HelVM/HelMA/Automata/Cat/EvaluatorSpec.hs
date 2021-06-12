module HelVM.HelMA.Automata.Cat.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.Cat.Evaluator

import HelVM.HelMA.Common.IO.MockIO
import HelVM.HelMA.Common.Util

import Test.Hspec

spec :: Spec
spec = do

  describe "monadic" $ do
    it "Hello , world!" $ do (batchExecMockIO . eval) hw `shouldBe` hw

hw :: Source
hw = "#!/bin/cat\nHello , world!\n"