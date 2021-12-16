module HelVM.HelMA.Automata.Rev.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.Rev.Evaluator

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.MockIO

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "output" $
    it "Hello, world!" $ (calculateOutput . execMockIOBatch . eval) hw `shouldBe` hwo

hw :: Source
hw = "#!/usr/bin/rev\n!dlrow ,olleH\n"

hwo :: Output
hwo = "ver/nib/rsu/!#\nHello, world!\n"
